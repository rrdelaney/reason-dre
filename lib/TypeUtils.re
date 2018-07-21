open Flow_parser;
open Ast_404;

exception TypeNotSupported(Loc.t);
exception ObjectFieldNotSupported(Loc.t);
exception TypeNotInScope(string, Loc.t);
exception TypeVarsMustBeLowercase(string, Loc.t);

let loc = AstUtils.loc;

let rec convertType =
        (~scope, (loc, t): Ast.Type.t(Loc.t))
        : Parsetree.core_type =>
  switch (t) {
  | Number => AstUtils.makeNamedType("float")
  | String => AstUtils.makeNamedType("string")
  | Boolean => AstUtils.makeNamedType("boolean")
  | Void => AstUtils.makeNamedType("unit")

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
        ((loc, param)) => Ast.Type.Function.Param.(param.annot),
        concreteParams,
      );

    AstUtils.makeFunctionType(
      if (List.length(concreteParamTypes) > 0) {
        List.map(convertType(~scope=functionScope), concreteParamTypes);
      } else {
        [AstUtils.makeNamedType("unit")];
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
    let (loc, name) =
      switch (tt.id) {
      | Ast.Type.Generic.Identifier.Unqualified(id) => id
      | Ast.Type.Generic.Identifier.Qualified((loc, _)) =>
        raise(TypeNotSupported(loc))
      };

    switch (DynamicScope.get(name, scope)) {
    | _ when DynamicScope.is(name, scope) => AstUtils.makeNamedType("t")
    | Some(DynamicScope.TypeVariable(t)) => AstUtils.makeNamedTypeVar(t)
    | Some(DynamicScope.Named(t)) when CasingUtils.isFirstLetterLowercase(t) =>
      AstUtils.makeNamedType(t)
    | Some(DynamicScope.Named(t)) => AstUtils.makeNamedType(t ++ ".t")
    | None => raise(TypeNotInScope(name, loc))
    };

  | Array(t) => AstUtils.makeAppliedType("array", [convertType(~scope, t)])

  | Interface(tt) => raise(TypeNotSupported(loc))
  | Empty => raise(TypeNotSupported(loc))
  | Any => raise(TypeNotSupported(loc))
  | Mixed => raise(TypeNotSupported(loc))
  | Null => raise(TypeNotSupported(loc))
  | Nullable(tt) => raise(TypeNotSupported(loc))
  | Union(a, b, c) => raise(TypeNotSupported(loc))
  | Intersection(a, b, c) => raise(TypeNotSupported(loc))
  | Typeof(tt) => raise(TypeNotSupported(loc))
  | Tuple(tt) => raise(TypeNotSupported(loc))
  | StringLiteral(tt) => raise(TypeNotSupported(loc))
  | NumberLiteral(tt) => raise(TypeNotSupported(loc))
  | BooleanLiteral(tt) => raise(TypeNotSupported(loc))
  | Exists => raise(TypeNotSupported(loc))
  };

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
                   ~scope=DynamicScope.withName(interfaceName, scope),
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
         | SpreadProperty((loc, _))
         | Indexer((loc, _))
         | CallProperty((loc, _))
         | InternalSlot((loc, _)) => raise(ObjectFieldNotSupported(loc)),
       )
  );

let makeInterfaceDeclaration =
    (
      ~scope,
      ~interfaceName: string,
      ~interfaceType: Flow_parser.Ast.Type.Object.t(Flow_parser.Loc.t),
    )
    : Parsetree.structure_item =>
  AstUtils.makeInterfaceDeclaration(
    ~name="t",
    ~fields=
      Ast.Type.Object.(
        interfaceType.properties
        |> List.filter(
             fun
             | Property((loc, {_method})) => ! _method
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
                       ~scope=DynamicScope.withName(interfaceName, scope),
                       t,
                     )
                   | Get((loc, _))
                   | Set((loc, _)) => raise(ObjectFieldNotSupported(loc))
                   };

                 (propName, propType);
               }
             | SpreadProperty((loc, _))
             | Indexer((loc, _))
             | CallProperty((loc, _))
             | InternalSlot((loc, _)) =>
               raise(ObjectFieldNotSupported(loc)),
           )
      ),
  );
