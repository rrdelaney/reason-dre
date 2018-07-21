open Flow_parser;
open Ast_404;

exception ParseError(list((Loc.t, Parser_common.Error.t)));
exception ModuleNameMustBeStringLiteral(Loc.t);
exception VarMustHaveType(Loc.t);
exception TypeAliasNameMustBeLowercase(string, Loc.t);
exception InterfaceNameMustBeUppercase(string, Loc.t);

type file = {
  source: string,
  filename: string,
};

let rec handleStatement = (~scope, (loc, statement)) : Parsetree.structure =>
  switch (statement) {
  | Ast.Statement.DeclareModule(m) =>
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

  | Ast.Statement.DeclareVariable(v) =>
    let (loc, varName) = v.id;
    let (_annotLoc, varType) =
      switch (v.annot) {
      | Some(annot) => annot
      | None => raise(VarMustHaveType(loc))
      };

    [
      AstUtils.makeExtern(
        ~moduleName=scope.moduleName,
        ~defaultExport=false,
        ~externName=varName,
        ~externType=TypeUtils.convertType(~scope, varType),
      ),
    ];

  | Ast.Statement.DeclareFunction(f) =>
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

  | Ast.Statement.DeclareTypeAlias(t) =>
    let (nameLoc, aliasName) = t.id;
    if (! CasingUtils.isFirstLetterLowercase(aliasName)) {
      raise(InterfaceNameMustBeUppercase(aliasName, nameLoc));
    };

    DynamicScope.push(DynamicScope.Named(aliasName), scope);

    let aliasType = t.right;

    [
      AstUtils.makeTypeDeclaration(
        ~aliasName,
        ~aliasType=TypeUtils.convertType(~scope, aliasType),
      ),
    ];

  | Ast.Statement.DeclareInterface(i) =>
    let (nameLoc, ifaceName) = i.id;
    if (! CasingUtils.isFirstLetterUppercase(ifaceName)) {
      raise(InterfaceNameMustBeUppercase(ifaceName, nameLoc));
    };

    DynamicScope.push(DynamicScope.Named(ifaceName), scope);

    let (_ifaceLoc, ifaceType) = i.body;

    [
      AstUtils.makeModule(
        ifaceName,
        [
          TypeUtils.makeInterfaceDeclaration(
            ~scope,
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
