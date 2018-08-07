open Flow_parser;
open Ast_404;

exception ParseError(list((Loc.t, Parser_common.Error.t)));
exception ModuleNameMustBeStringLiteral(Loc.t);
exception VarMustHaveType(Loc.t);
exception TypeAliasNameMustBeLowercase(string, Loc.t);
exception TypeNameNameMustBeLowercase(string, Loc.t);
exception InterfaceNameMustBeUppercase(string, Loc.t);
exception ClassNameMustBeUppercase(string, Loc.t);
exception ModuleExportsMustBeInModule(Loc.t);

type file = {
  source: string,
  filename: string,
};

let rec handleDeclareModule = (~scope: DynamicScope.scope, ~loc, m) => {
  open Ast.Statement.DeclareModule;

  let moduleName =
    switch (m.id) {
    | Ast.Statement.DeclareModule.Identifier(_) =>
      raise(ModuleNameMustBeStringLiteral(loc))
    | Ast.Statement.DeclareModule.Literal((loc, literal)) => literal.value
    };

  let moduleScope = DynamicScope.withModule(moduleName, scope);

  let (loc, moduleBody) = m.body;
  let body = moduleBody.body;
  body |> List.map(handleStatement(~scope=moduleScope)) |> List.flatten;
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
        ~moduleName=scope.moduleName,
        ~defaultExport=false,
        ~externName=varName,
        ~externType=TypeUtils.convertType(~scope, varType),
      ),
    ]
  };
}
and handleDeclareFunction = (~scope: DynamicScope.scope, ~loc, f) => {
  open Ast.Statement.DeclareFunction;

  let (_fnameLoc, functionName) = f.id;
  let (_annotLoc, functionType) = f.annot;

  [
    AstUtils.makeExtern(
      ~moduleName=scope.moduleName,
      ~defaultExport=false,
      ~externName=functionName,
      ~externType=
        TypeUtils.convertType(
          ~scope=DynamicScope.clone(scope),
          functionType,
        ),
    ),
  ];
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
    switch (scope.moduleName) {
    | Some(name) => CasingUtils.makeVariableName(name)
    | None => raise(ModuleExportsMustBeInModule(loc))
    };

  let externType = TypeUtils.convertType(~scope, t);

  [
    AstUtils.makeExtern(
      ~moduleName=scope.moduleName,
      ~defaultExport=true,
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
and handleStatement = (~scope, (loc, statement)) : Parsetree.structure =>
  switch (statement) {
  | Ast.Statement.DeclareModule(m) => handleDeclareModule(~scope, ~loc, m)
  | Ast.Statement.DeclareVariable(v) => handleDeclareVariable(~scope, ~loc, v)
  | Ast.Statement.DeclareFunction(f) => handleDeclareFunction(~scope, ~loc, f)
  | Ast.Statement.DeclareTypeAlias(t) =>
    handleDeclareTypeAlias(~scope, ~loc, t)
  | Ast.Statement.DeclareInterface(i) =>
    handleDeclareInterface(~scope, ~loc, i)
  | Ast.Statement.DeclareClass(c) => handleDeclareClass(~scope, ~loc, c)
  | Ast.Statement.DeclareModuleExports(m) =>
    handleDeclareModuleExports(~scope, ~loc, m)
  | Ast.Statement.DeclareOpaqueType(t) =>
    handleDeclareOpaqueType(~scope, ~loc, t)
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
    |> List.map(handleStatement(~scope=programScope))
    |> List.flatten;

  Reason_toolchain.RE.print_implementation_with_comments(
    Format.str_formatter,
    (program, []),
  );

  Format.flush_str_formatter();
};
