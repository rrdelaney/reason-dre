open Flow_parser;
open Ast_404;

exception TypeNotSupported(Loc.t);
exception ObjectFieldNotSupported(Loc.t);
exception TypeNotInScope(string, Loc.t);
exception TypeVarsMustBeLowercase(string, Loc.t);
exception NotEnoughTypeArguments(string, int, int, Loc.t);
exception TypeofMustBeOfClass(Loc.t);

let loc = AstUtils.loc;

let rec extractNameFromGenericId = id =>
  switch (id) {
  | Ast.Type.Generic.Identifier.Unqualified((_, name)) => name
  | Ast.Type.Generic.Identifier.Qualified((
      loc,
      {qualification, id: (_, name)},
    )) =>
    extractNameFromGenericId(qualification) ++ "." ++ name
  };

let rec convertType =
        (~scope, (loc, t): Ast.Type.t(Loc.t))
        : Parsetree.core_type =>
  switch (t) {
  | Number when DreConfig.strictTypes => raise(TypeNotInScope("number", loc))
  | Number => AstUtils.makeNamedType("float")
  | Void when DreConfig.strictTypes => raise(TypeNotInScope("void", loc))
  | Void => AstUtils.makeNamedType("unit")
  | Boolean when DreConfig.strictTypes =>
    raise(TypeNotInScope("boolean", loc))
  | Boolean => AstUtils.makeNamedType("bool")
  | String => AstUtils.makeNamedType("string")

  | Ast.Type.Function(f) =>
    let (_loc, paramTypes) = f.params;
    let (retLoc, returnType) = f.return;
    let concreteParams = paramTypes.params;
    let typeParams =
      switch (f.tparams) {
      | Some((_loc, params)) => params
      | None => []
      };

    let functionScope = DynamicScope.clone(scope);
    List.iter(
      ((_, param): Ast.Type.ParameterDeclaration.TypeParam.t(Loc.t)) => {
        let (loc, name) = param.name;
        if (! CasingUtils.isFirstLetterLowercase(name)) {
          raise(TypeVarsMustBeLowercase(name, loc));
        };

        DynamicScope.push(DynamicScope.TypeVariable(name), functionScope);
      },
      typeParams,
    );

    let concreteParamTypes =
      List.map(
        ((loc, param): Ast.Type.Function.Param.t(Loc.t)) => {
          let paramType = convertType(~scope=functionScope, param.annot);
          let optionalLabel =
            switch (param.optional, param.name) {
            | (true, Some((loc, name))) => Some(name)
            | _ => None
            };
          (optionalLabel, paramType);
        },
        concreteParams,
      );

    let hasOptional =
      List.exists(
        ((label, _)) =>
          switch (label) {
          | Some(_) => true
          | None => false
          },
        concreteParamTypes,
      );

    let concreteParamTypes =
      if (hasOptional) {
        List.concat([
          concreteParamTypes,
          [(None, AstUtils.makeNamedType("unit"))],
        ]);
      } else {
        concreteParamTypes;
      };

    AstUtils.makeFunctionType(
      if (List.length(concreteParamTypes) > 0) {
        concreteParamTypes;
      } else {
        [(None, AstUtils.makeNamedType("unit"))];
      },
      convertType(~scope=functionScope, (retLoc, returnType)),
    );

  | Object(tt) =>
    let objProps =
      Ast.Type.Object.(
        List.map(
          fun
          | Property((loc, prop)) => {
              open Ast.Type.Object.Property;
              let {key, value} = prop;

              let propName =
                switch (key) {
                | Ast.Expression.Object.Property.Identifier((loc, id)) => id

                | Ast.Expression.Object.Property.Literal((loc, _))
                | Ast.Expression.Object.Property.PrivateName((loc, _))
                | Ast.Expression.Object.Property.Computed((loc, _)) =>
                  raise(ObjectFieldNotSupported(loc))
                };
              let propType =
                switch (value) {
                | Init(t) => convertType(~scope, t)
                | Get((loc, _))
                | Set((loc, _)) => raise(ObjectFieldNotSupported(loc))
                };

              (propName, propType);
            }

          | SpreadProperty((loc, _))
          | Indexer((loc, _))
          | CallProperty((loc, _))
          | InternalSlot((loc, _)) => raise(ObjectFieldNotSupported(loc)),
          tt.properties,
        )
      );

    AstUtils.makeObjectType(objProps);

  | Generic(tt) =>
    let name = extractNameFromGenericId(tt.id);

    let typeArgsLoc =
      switch (tt.targs) {
      | Some((loc, _)) => loc
      | None => Loc.none
      };

    let typeArgs =
      switch (tt.targs) {
      | Some((loc, typeArgs)) =>
        Some(List.map(convertType(~scope), typeArgs))
      | None => None
      };

    let typeArgCount =
      switch (typeArgs) {
      | Some(ts) => List.length(ts)
      | None => 0
      };

    let applyTypeArgs =
      switch (typeArgs) {
      | Some(ts) => (t => AstUtils.makeAppliedType(t, ts))
      | None => (t => AstUtils.makeNamedType(t))
      };

    switch (DynamicScope.get(name, scope)) {
    | _ when DynamicScope.is(name, scope) => applyTypeArgs("t")
    | Some(DynamicScope.TypeVariable(t)) => AstUtils.makeNamedTypeVar(t)
    | Some(DynamicScope.TypeAlias({name})) => applyTypeArgs(name)
    | Some(DynamicScope.Interface({name, typeParamCount}))
        when typeParamCount != typeArgCount =>
      raise(
        NotEnoughTypeArguments(
          name,
          typeParamCount,
          typeArgCount,
          typeArgsLoc,
        ),
      )
    | Some(DynamicScope.Interface({name})) => applyTypeArgs(name ++ ".t")
    | Some(DynamicScope.BuiltIn({reasonName})) => applyTypeArgs(reasonName)
    | Some(DynamicScope.ClassDef({name})) => applyTypeArgs(name ++ ".t")
    | None => raise(TypeNotInScope(name, loc))
    };

  | Array(t) => AstUtils.makeAppliedType("array", [convertType(~scope, t)])

  | Mixed => AstUtils.makeNamedType("Js.Json.t")

  | Typeof(tt) => raise(TypeNotSupported(loc))
  | Interface(tt) => raise(TypeNotSupported(loc))
  | Empty => raise(TypeNotSupported(loc))
  | Any => raise(TypeNotSupported(loc))
  | Null => raise(TypeNotSupported(loc))
  | Nullable(tt) => raise(TypeNotSupported(loc))
  | Union(a, b, c) => raise(TypeNotSupported(loc))
  | Intersection(a, b, c) => raise(TypeNotSupported(loc))
  | Tuple(tt) => raise(TypeNotSupported(loc))
  | StringLiteral(tt) => raise(TypeNotSupported(loc))
  | NumberLiteral(tt) => raise(TypeNotSupported(loc))
  | BooleanLiteral(tt) => raise(TypeNotSupported(loc))
  | Exists => raise(TypeNotSupported(loc))
  };

let makeConstructor =
    (
      ~scope,
      ~bindingName,
      ~interfaceName,
      ~interfaceType: Flow_parser.Ast.Type.Object.t(Flow_parser.Loc.t),
    )
    : Parsetree.structure_item =>
  Ast.Type.Object.(
    interfaceType.properties
    |> List.filter(
         fun
         | Property((loc, {key: Identifier((_, "constructor"))})) => true
         | _ => false,
       )
    |> List.map(
         fun
         | Property((loc, prop)) => {
             open Ast.Type.Object.Property;

             let {value} = prop;

             let propType =
               switch (value) {
               | Init(t) =>
                 convertType(
                   ~scope=
                     DynamicScope.withInterface(
                       ~name=interfaceName,
                       ~typeParamCount=0,
                       scope,
                     ),
                   t,
                 )
               | Get((loc, _))
               | Set((loc, _)) => raise(ObjectFieldNotSupported(loc))
               };

             AstUtils.makeNewExtern(
               ~moduleName=scope.moduleName,
               ~localName="make",
               ~externName=bindingName,
               ~externType=propType,
             );
           }
         | SpreadProperty((loc, _))
         | Indexer((loc, _))
         | CallProperty((loc, _))
         | InternalSlot((loc, _)) => raise(ObjectFieldNotSupported(loc)),
       )
    |> List.hd
  );

let makeMethods =
    (
      ~scope,
      ~interfaceName,
      ~interfaceType: Flow_parser.Ast.Type.Object.t(Flow_parser.Loc.t),
    )
    : list(Parsetree.structure_item) =>
  Ast.Type.Object.(
    interfaceType.properties
    |> List.filter(
         fun
         | Property((loc, {key: Identifier((_, "constructor"))})) => false
         | Property((loc, {_method})) => _method
         | SpreadProperty((loc, _))
         | Indexer((loc, _))
         | CallProperty((loc, _))
         | InternalSlot((loc, _)) => true,
       )
    |> List.map(
         fun
         | Property((loc, prop)) => {
             open Ast.Type.Object.Property;
             let {key, value} = prop;

             let propName =
               switch (key) {
               | Ast.Expression.Object.Property.Identifier((loc, id)) => id

               | Ast.Expression.Object.Property.Literal((loc, _))
               | Ast.Expression.Object.Property.PrivateName((loc, _))
               | Ast.Expression.Object.Property.Computed((loc, _)) =>
                 raise(ObjectFieldNotSupported(loc))
               };

             let propType =
               switch (value) {
               | Init(t) =>
                 convertType(
                   ~scope=
                     DynamicScope.withInterface(
                       ~name=interfaceName,
                       ~typeParamCount=0,
                       scope,
                     ),
                   t,
                 )
               | Get((loc, _))
               | Set((loc, _)) => raise(ObjectFieldNotSupported(loc))
               };

             AstUtils.makeMethodExtern(
               ~methodName=propName,
               ~methodType=propType,
             );
           }

         | Indexer((loc, t)) => {
             let converterType =
               AstUtils.makeFunctionType(
                 [(None, AstUtils.makeNamedType("t"))],
                 convertType(~scope, t.value),
               );

             AstUtils.makeIdentityExtern(
               ~externName="asDict",
               ~externType=converterType,
             );
           }

         | SpreadProperty((loc, _))
         | CallProperty((loc, _))
         | InternalSlot((loc, _)) => raise(ObjectFieldNotSupported(loc)),
       )
  );

let makeInterfaceDeclaration =
    (
      ~scope,
      ~interfaceName: string,
      ~interfaceType: Flow_parser.Ast.Type.Object.t(Flow_parser.Loc.t),
      ~typeParamNames: list(string),
    )
    : Parsetree.structure_item =>
  AstUtils.makeInterfaceDeclaration(
    ~name="t",
    ~typeParamNames,
    ~fields=
      Ast.Type.Object.(
        interfaceType.properties
        |> List.filter(
             fun
             | Property((loc, {_method})) => ! _method
             | Indexer((loc, _)) => false
             | SpreadProperty((loc, _))
             | CallProperty((loc, _))
             | InternalSlot((loc, _)) => true,
           )
        |> List.map(
             fun
             | Property((loc, prop)) => {
                 open Ast.Type.Object.Property;
                 let {key, value, optional} = prop;

                 let propName =
                   switch (key) {
                   | Ast.Expression.Object.Property.Identifier((loc, id)) => id

                   | Ast.Expression.Object.Property.Literal((loc, _))
                   | Ast.Expression.Object.Property.PrivateName((loc, _))
                   | Ast.Expression.Object.Property.Computed((loc, _)) =>
                     raise(ObjectFieldNotSupported(loc))
                   };

                 let propType =
                   switch (value) {
                   | Init(t) =>
                     convertType(
                       ~scope=
                         DynamicScope.withInterface(
                           ~name=interfaceName,
                           ~typeParamCount=0,
                           scope,
                         ),
                       t,
                     )
                   | Get((loc, _))
                   | Set((loc, _)) => raise(ObjectFieldNotSupported(loc))
                   };

                 (propName, optional, propType);
               }

             | Indexer((loc, _))
             | SpreadProperty((loc, _))
             | CallProperty((loc, _))
             | InternalSlot((loc, _)) =>
               raise(ObjectFieldNotSupported(loc)),
           )
      ),
  );
