open Flow_parser;
open Ast_404;

exception TypeNotSupported(Loc.t);
exception TypeNameNotSupported(Loc.t);
exception ObjectFieldNotSupported(Loc.t);
exception TypeNotInScope(string, Loc.t);
exception TypeVarsMustBeLowercase(string, Loc.t);
exception NotEnoughTypeArguments(string, int, int, Loc.t);
exception TypeofMustBeOfClass(Loc.t);
exception CouldNotBuildName(Loc.t);

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

let rec buildTypeName =
        ((loc, t): Ast.Type.t(Loc.t))
        : DiffableTree.node(string) =>
  switch (t) {
  | Number => {value: "number", children: []}
  | Void => {value: "void", children: []}
  | Boolean => {value: "boolean", children: []}
  | String => {value: "string", children: []}
  | Function(f) =>
    let (_loc, paramTypes) = f.params;
    {
      value: "function",
      children:
        List.map(
          ((_loc, t): Ast.Type.Function.Param.t(Loc.t)) =>
            buildTypeName(t.annot),
          paramTypes.params,
        ),
    };

  | Generic(tt) => {value: extractNameFromGenericId(tt.id), children: []}

  | StringLiteral({value}) => {value, children: []}

  | _ => raise(TypeNameNotSupported(loc))
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

  | Function(f) =>
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

    let hasOnlyStringLiterals =
      List.for_all(
        ((loc, param): Ast.Type.Function.Param.t(Loc.t)) =>
          switch (param.annot) {
          | (_, Ast.Type.StringLiteral(_)) => true
          | _ => false
          },
        concreteParams,
      );

    let concreteParamTypes =
      if (hasOptional || hasOnlyStringLiterals) {
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
                | Init(t) => convertType(~scope, t)
                | Get((loc, _))
                | Set((loc, _)) => raise(ObjectFieldNotSupported(loc))
                };

              let propWithNullable =
                if (optional) {
                  AstUtils.makeAppliedType("Js.nullable", [propType]);
                } else {
                  propType;
                };

              (propName, propWithNullable);
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

  | Nullable(t) =>
    AstUtils.makeAppliedType("Js.nullable", [convertType(~scope, t)])

  | StringLiteral({value}) => AstUtils.makeFixedStringType(value)

  | Typeof(tt) => raise(TypeNotSupported(loc))
  | Interface(tt) => raise(TypeNotSupported(loc))
  | Empty => raise(TypeNotSupported(loc))
  | Any => raise(TypeNotSupported(loc))
  | Null => raise(TypeNotSupported(loc))
  | Union(a, b, c) => raise(TypeNotSupported(loc))
  | Intersection(a, b, c) => raise(TypeNotSupported(loc))
  | Tuple(tt) => raise(TypeNotSupported(loc))
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
               ~moduleName=DynamicScope.moduleName(scope),
               ~namespaces=DynamicScope.namespaces(scope),
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
    : list(Parsetree.structure_item) => {
  open Ast.Type.Object;

  let makeAsDictMethod = (t: Ast.Type.Object.Indexer.t'(Loc.t)) => {
    let converterType =
      AstUtils.makeFunctionType(
        [(None, AstUtils.makeNamedType("t"))],
        convertType(~scope, t.value),
      );

    AstUtils.makeIdentityExtern(
      ~externName="asDict",
      ~externType=converterType,
    );
  };

  let getPropName = ({key}: Ast.Type.Object.Property.t'(Loc.t)) =>
    switch (key) {
    | Ast.Expression.Object.Property.Identifier((loc, id)) => id
    | Ast.Expression.Object.Property.Literal((loc, _))
    | Ast.Expression.Object.Property.PrivateName((loc, _))
    | Ast.Expression.Object.Property.Computed((loc, _)) =>
      raise(ObjectFieldNotSupported(loc))
    };

  let getPropFlowType = ({value}: Ast.Type.Object.Property.t'(Loc.t)) =>
    switch (value) {
    | Init(t) => t
    | Get((loc, _))
    | Set((loc, _)) => raise(ObjectFieldNotSupported(loc))
    };

  let getPropType = ({value}: Ast.Type.Object.Property.t'(Loc.t)) =>
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

  let methods =
    List.filter(
      fun
      | Property((loc, {key: Identifier((_, "constructor"))})) => false
      | Property((loc, {_method})) => _method
      | SpreadProperty((loc, _))
      | Indexer((loc, _))
      | CallProperty((loc, _))
      | InternalSlot((loc, _)) => true,
      interfaceType.properties,
    );

  MutableIterator.map(
    (~iter, meth) =>
      switch (meth) {
      | Property((loc, {key: Identifier((_, "$DRE_INTERNAL_SKIP"))})) => []
      | Property((loc, prop)) =>
        let propName = getPropName(prop);
        let propType = getPropType(prop);

        let overloads = {
          let overloads = ref([]);
          for (i in 0 to iter.length - 1) {
            switch (iter.peek(i)) {
            | Property((
                overloadLoc,
                {key: Ast.Expression.Object.Property.Identifier((loc, id))} as overloadProp,
              ))
                when id == propName =>
              iter.replace(
                i,
                Property((
                  overloadLoc,
                  {
                    ...overloadProp,
                    key:
                      Ast.Expression.Object.Property.Identifier((
                        loc,
                        "$DRE_INTERNAL_SKIP",
                      )),
                  },
                )),
              );
              overloads := [overloadProp, ...overloads^];
            | _ => ()
            };
          };

          overloads^;
        };

        [
          if (List.length(overloads) > 0) {
            let firstFuncName = buildTypeName(getPropFlowType(prop));

            AstUtils.makeModule(
              CasingUtils.makeModuleName(propName),
              [prop, ...overloads]
              |> List.mapi((i, f) => {
                   let overrideName =
                     if (i == 0) {
                       let pft = getPropFlowType(List.nth(overloads, 0));
                       let overrideName = buildTypeName(pft);
                       switch (DiffableTree.diff(overrideName, firstFuncName)) {
                       | Some(name) => name
                       | None => raise(CouldNotBuildName(loc))
                       };
                     } else {
                       let pft = getPropFlowType(f);
                       let overrideName = buildTypeName(pft);
                       switch (DiffableTree.diff(firstFuncName, overrideName)) {
                       | Some(name) => name
                       | None => raise(CouldNotBuildName(loc))
                       };
                     };
                   AstUtils.makeMethodExtern(
                     ~bindingName=Some(overrideName),
                     ~methodName=getPropName(f),
                     ~methodType=getPropType(f),
                   );
                 }),
            );
          } else {
            AstUtils.makeMethodExtern(
              ~bindingName=None,
              ~methodName=propName,
              ~methodType=propType,
            );
          },
        ];

      | Indexer((loc, t)) => [makeAsDictMethod(t)]
      | SpreadProperty((loc, _))
      | CallProperty((loc, _))
      | InternalSlot((loc, _)) => raise(ObjectFieldNotSupported(loc))
      },
    methods,
  )
  |> List.flatten;
};

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
