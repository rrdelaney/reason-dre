open Flow_parser;
open Ast_404;

exception ParseError(list((Loc.t, Parser_common.Error.t)));
exception ModuleNameMustBeStringLiteral(Loc.t);
exception VarMustHaveType(Loc.t);

type file = {
  source: string,
  filename: string,
};

let rec handleStatement =
        (~moduleName="", (loc, statement))
        : Parsetree.structure =>
  switch (statement) {
  | Ast.Statement.DeclareModule(m) =>
    let moduleName =
      switch (m.id) {
      | Ast.Statement.DeclareModule.Identifier(_) =>
        raise(ModuleNameMustBeStringLiteral(loc))
      | Ast.Statement.DeclareModule.Literal((loc, literal)) => literal.value
      };

    let (loc, moduleBody) = m.body;
    let body = moduleBody.body;
    body |> List.map(handleStatement(~moduleName)) |> List.flatten;

  | Ast.Statement.DeclareVariable(v) =>
    let (loc, varName) = v.id;
    let (_annotLoc, varType) =
      switch (v.annot) {
      | Some(annot) => annot
      | None => raise(VarMustHaveType(loc))
      };

    [
      AstUtils.makeExtern(
        ~moduleName,
        ~defaultExport=false,
        ~externName=varName,
        ~externType=TypeUtils.convertType(varType),
      ),
    ];

  | Ast.Statement.DeclareFunction(f) =>
    let (_fnameLoc, functionName) = f.id;
    let (_annotLoc, functionType) = f.annot;

    [
      AstUtils.makeExtern(
        ~moduleName,
        ~defaultExport=false,
        ~externName=functionName,
        ~externType=TypeUtils.convertType(functionType),
      ),
    ];

  | Ast.Statement.DeclareTypeAlias(t) =>
    let (_nameLoc, aliasName) = t.id;
    let aliasType = t.right;

    [
      AstUtils.makeTypeDeclaration(
        ~aliasName,
        ~aliasType=TypeUtils.convertType(aliasType),
      ),
    ];

  | Ast.Statement.DeclareInterface(i) =>
    let (_nameLoc, ifaceName) = i.id;
    let (_ifaceLoc, ifaceType) = i.body;

    [
      TypeUtils.makeInterfaceDeclaration(
        ~interfaceName=ifaceName,
        ~interfaceType=ifaceType,
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

  let program = statements |> List.map(handleStatement) |> List.flatten;

  Reason_toolchain.RE.print_implementation_with_comments(
    Format.str_formatter,
    (program, []),
  );

  Format.flush_str_formatter();
};
