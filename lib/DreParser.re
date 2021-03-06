open Flow_parser;
open Ast_404;

exception ParseError(list((Loc.t, Parser_common.Error.t)));
exception VarMustHaveType(Loc.t);
exception TypeAliasNameMustBeLowercase(string, Loc.t);
exception TypeNameNameMustBeLowercase(string, Loc.t);
exception InterfaceNameMustBeUppercase(string, Loc.t);
exception ClassNameMustBeUppercase(string, Loc.t);
exception ModuleExportsMustBeInModule(Loc.t);
exception WithStatementMustUseIdentifier(Loc.t);
exception WithStatementMustHaveABlock(Loc.t);

type file = {
  source: string,
  filename: string,
};

let rec handleDeclareModule = (~scope: DynamicScope.scope, ~loc, m) => {
  open Ast.Statement.DeclareModule;

  let moduleScope =
    switch (m.id) {
    | Ast.Statement.DeclareModule.Identifier((loc, name)) =>
      DynamicScope.withNamespace(name, scope)
    | Ast.Statement.DeclareModule.Literal((loc, literal)) =>
      DynamicScope.withNodeModule(literal.value, scope)
    };

  let (loc, moduleBody) = m.body;
  let body = moduleBody.body;

  body
  |> MutableIterator.map(handleStatement(~scope=moduleScope))
  |> List.flatten;
}
and handleDeclareVariable = (~scope: DynamicScope.scope, ~loc, v) => {
  open Ast.Statement.DeclareVariable;

  let (loc, varName) = v.id;
  let (_annotLoc, varType) =
    switch (v.annot) {
    | Some(annot) => annot
    | None => raise(VarMustHaveType(loc))
    };

  switch (varType) {
  | (loc, Ast.Type.Typeof((_, Ast.Type.Generic(tt)))) =>
    let (loc, name) =
      switch (tt.id) {
      | Ast.Type.Generic.Identifier.Unqualified(id) => id
      | Ast.Type.Generic.Identifier.Qualified((loc, _)) =>
        raise(TypeUtils.TypeNotSupported(loc))
      };

    switch (DynamicScope.get(name, scope)) {
    | Some(DynamicScope.ClassDef({make})) => [make(varName)]
    | _ => raise(TypeUtils.TypeofMustBeOfClass(loc))
    };

  | _ => [
      AstUtils.makeExtern(
        ~moduleName=DynamicScope.moduleName(scope),
        ~namespaces=DynamicScope.namespaces(scope),
        ~defaultExport=false,
        ~bindingName=None,
        ~externName=varName,
        ~externType=TypeUtils.convertType(~scope, varType),
      ),
    ]
  };
}
and handleDeclareFunction =
    (
      ~iter: MutableIterator.iter(Ast.Statement.t(Loc.t)),
      ~scope: DynamicScope.scope,
      ~loc,
      ~overrideName=?,
      f,
    ) => {
  open Ast.Statement.DeclareFunction;

  let (_fnameLoec, functionName) = f.id;
  let (_annotLoc, functionType) = f.annot;

  let overloads = {
    let overloads = ref([]);
    for (i in 0 to iter.length - 1) {
      switch (iter.peek(i)) {
      | (_, Ast.Statement.DeclareFunction({id: (_, overloadName)} as f))
          when overloadName == functionName =>
        iter.replace(i, (loc, Ast.Statement.Empty));
        overloads := [f, ...overloads^];
      | _ => ()
      };
    };

    overloads^;
  };

  if (List.length(overloads) > 0) {
    let firstFuncName = TypeUtils.buildTypeName(functionType);

    [
      AstUtils.makeModule(
        CasingUtils.makeModuleName(functionName),
        [f, ...overloads]
        |> List.mapi((i, f) => {
             let overrideName =
               if (i == 0) {
                 let (loc, ft) = List.nth(overloads, 0).annot;
                 let overrideName = TypeUtils.buildTypeName(ft);
                 switch (DiffableTree.diff(overrideName, firstFuncName)) {
                 | Some(name) => name
                 | None => raise(TypeUtils.CouldNotBuildName(loc))
                 };
               } else {
                 let (loc, ft) = f.annot;
                 let overrideName = TypeUtils.buildTypeName(ft);
                 switch (DiffableTree.diff(firstFuncName, overrideName)) {
                 | Some(name) => name
                 | None => raise(TypeUtils.CouldNotBuildName(loc))
                 };
               };
             handleDeclareFunction(~iter, ~scope, ~loc, ~overrideName, f);
           })
        |> List.flatten,
      ),
    ];
  } else {
    [
      AstUtils.makeExtern(
        ~moduleName=DynamicScope.moduleName(scope),
        ~namespaces=DynamicScope.namespaces(scope),
        ~defaultExport=false,
        ~bindingName=overrideName,
        ~externName=functionName,
        ~externType=
          TypeUtils.convertType(
            ~scope=DynamicScope.clone(scope),
            functionType,
          ),
      ),
    ];
  };
}
and handleDeclareTypeAlias = (~scope: DynamicScope.scope, ~loc, t) => {
  open Ast.Statement.TypeAlias;

  let (nameLoc, aliasName) = t.id;
  if (! CasingUtils.isFirstLetterLowercase(aliasName)) {
    raise(InterfaceNameMustBeUppercase(aliasName, nameLoc));
  };

  DynamicScope.push(
    DynamicScope.TypeAlias({name: aliasName, typeParamCount: 0}),
    scope,
  );

  let aliasType = t.right;

  [
    AstUtils.makeTypeDeclaration(
      ~aliasName,
      ~aliasType=TypeUtils.convertType(~scope, aliasType),
    ),
  ];
}
and handleDeclareInterface = (~scope: DynamicScope.scope, ~loc, i) => {
  open Ast.Statement.Interface;

  let (nameLoc, ifaceName) = i.id;
  if (! CasingUtils.isFirstLetterUppercase(ifaceName)) {
    raise(InterfaceNameMustBeUppercase(ifaceName, nameLoc));
  };

  let tParams =
    switch (i.tparams) {
    | Some((loc, params)) => params
    | None => []
    };

  let typeParamNames =
    List.map(
      ((loc, param): Ast.Type.ParameterDeclaration.TypeParam.t(Loc.t)) => {
        let (_, name) = param.name;
        if (! CasingUtils.isFirstLetterLowercase(name)) {
          raise(TypeUtils.TypeVarsMustBeLowercase(name, loc));
        };

        name;
      },
      tParams,
    );

  List.iter(
    paramName =>
      DynamicScope.push(DynamicScope.TypeVariable(paramName), scope),
    typeParamNames,
  );

  DynamicScope.push(
    DynamicScope.Interface({
      name: ifaceName,
      typeParamCount: List.length(typeParamNames),
    }),
    scope,
  );

  let (_ifaceLoc, ifaceType) = i.body;

  [
    AstUtils.makeModule(
      ifaceName,
      [
        TypeUtils.makeInterfaceDeclaration(
          ~scope,
          ~typeParamNames,
          ~interfaceName=ifaceName,
          ~interfaceType=ifaceType,
        ),
        ...TypeUtils.makeMethods(
             ~scope,
             ~interfaceName=ifaceName,
             ~interfaceType=ifaceType,
           ),
      ],
    ),
  ];
}
and handleDeclareClass = (~scope: DynamicScope.scope, ~loc, c) => {
  open Ast.Statement.DeclareClass;

  let (nameLoc, className) = c.id;
  if (! CasingUtils.isFirstLetterUppercase(className)) {
    raise(ClassNameMustBeUppercase(className, nameLoc));
  };

  let tParams =
    switch (c.tparams) {
    | Some((loc, params)) => params
    | None => []
    };

  let typeParamNames =
    List.map(
      ((loc, param): Ast.Type.ParameterDeclaration.TypeParam.t(Loc.t)) => {
        let (_, name) = param.name;
        if (! CasingUtils.isFirstLetterLowercase(name)) {
          raise(TypeUtils.TypeVarsMustBeLowercase(name, loc));
        };

        name;
      },
      tParams,
    );

  List.iter(
    paramName =>
      DynamicScope.push(DynamicScope.TypeVariable(paramName), scope),
    typeParamNames,
  );

  let (_classLoc, classType) = c.body;

  DynamicScope.push(
    DynamicScope.ClassDef({
      name: className,
      typeParamCount: List.length(typeParamNames),
      make: binding =>
        AstUtils.makeModule(
          className,
          [
            TypeUtils.makeInterfaceDeclaration(
              ~scope,
              ~typeParamNames,
              ~interfaceName=className,
              ~interfaceType=classType,
            ),
            TypeUtils.makeConstructor(
              ~scope,
              ~bindingName=binding,
              ~interfaceName=className,
              ~interfaceType=classType,
            ),
            ...TypeUtils.makeMethods(
                 ~scope,
                 ~interfaceName=className,
                 ~interfaceType=classType,
               ),
          ],
        ),
    }),
    scope,
  );

  [];
}
and handleDeclareModuleExports =
    (~scope: DynamicScope.scope, ~loc, (tloc, t)) => {
  let externName =
    switch (DynamicScope.moduleName(scope)) {
    | Some(name) => CasingUtils.makeVariableName(name)
    | None => raise(ModuleExportsMustBeInModule(loc))
    };

  let externType = TypeUtils.convertType(~scope, t);

  [
    AstUtils.makeExtern(
      ~moduleName=DynamicScope.moduleName(scope),
      ~namespaces=DynamicScope.namespaces(scope),
      ~defaultExport=true,
      ~bindingName=None,
      ~externName,
      ~externType,
    ),
  ];
}
and handleDeclareOpaqueType =
    (~scope, ~loc, t: Ast.Statement.OpaqueType.t(Loc.t)) => {
  let (loc, typeName) = t.id;

  if (! CasingUtils.isFirstLetterLowercase(typeName)) {
    raise(TypeNameNameMustBeLowercase(typeName, loc));
  };

  DynamicScope.push(
    DynamicScope.TypeAlias({name: typeName, typeParamCount: 0}),
    scope,
  );

  [AstUtils.makeBareType(~typeName)];
}
and handleWithStatement = (~scope, ~loc, w: Ast.Statement.With.t(Loc.t)) => {
  let name =
    switch (w._object) {
    | (loc, Ast.Expression.Identifier((_, name))) => name
    | _ => raise(WithStatementMustUseIdentifier(loc))
    };

  let namespaceScope = DynamicScope.withNamespace(name, scope);

  let body =
    switch (w.body) {
    | (loc, Ast.Statement.Block(b)) => b.body
    | _ => raise(WithStatementMustHaveABlock(loc))
    };

  body
  |> MutableIterator.map(handleStatement(~scope=namespaceScope))
  |> List.flatten;
}
and handleStatement =
    (
      ~iter: MutableIterator.iter(Ast.Statement.t(Loc.t)),
      ~scope,
      (loc, statement),
    )
    : Parsetree.structure =>
  switch (statement) {
  | Ast.Statement.DeclareModule(m) => handleDeclareModule(~scope, ~loc, m)
  | Ast.Statement.DeclareVariable(v) => handleDeclareVariable(~scope, ~loc, v)
  | Ast.Statement.DeclareFunction(f) =>
    handleDeclareFunction(~iter, ~scope, ~loc, f)
  | Ast.Statement.DeclareTypeAlias(t) =>
    handleDeclareTypeAlias(~scope, ~loc, t)
  | Ast.Statement.DeclareInterface(i) =>
    handleDeclareInterface(~scope, ~loc, i)
  | Ast.Statement.DeclareClass(c) => handleDeclareClass(~scope, ~loc, c)
  | Ast.Statement.DeclareModuleExports(m) =>
    handleDeclareModuleExports(~scope, ~loc, m)
  | Ast.Statement.DeclareOpaqueType(t) =>
    handleDeclareOpaqueType(~scope, ~loc, t)
  | Ast.Statement.With(w) => handleWithStatement(~scope, ~loc, w)
  | Ast.Statement.Empty => []
  | _ => []
  };

let parse = file => {
  let (ast, errors) =
    Parser_flow.program_file(
      file.source,
      Some(File_key.SourceFile(file.source)),
    );

  if (List.length(errors) > 0) {
    raise(ParseError(errors));
  };

  let (_, statements, _) = ast;

  let programScope = DynamicScope.make();
  List.iter(
    t => DynamicScope.push(DynamicScope.BuiltIn(t), programScope),
    DrePrelude.types,
  );

  let program =
    statements
    |> MutableIterator.map(handleStatement(~scope=programScope))
    |> List.flatten;

  Reason_toolchain.RE.print_implementation_with_comments(
    Format.str_formatter,
    (program, []),
  );

  Format.flush_str_formatter();
};
