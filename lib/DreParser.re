exception
  ParseErrors(list((Flow_parser.Loc.t, Flow_parser.Parser_common.Error.t)));
exception ModuleNameMustBeStringLiteral(Flow_parser.Loc.t);

type file = {
  source: string,
  filename: string,
};

let rec handleStatement =
        (~moduleName="", (loc, statement))
        : Ast_404.Parsetree.structure =>
  switch (statement) {
  | Flow_parser.Ast.Statement.DeclareModule(m) =>
    let moduleName =
      switch (m.id) {
      | Flow_parser.Ast.Statement.DeclareModule.Identifier(_) =>
        raise(ModuleNameMustBeStringLiteral(loc))
      | Flow_parser.Ast.Statement.DeclareModule.Literal((loc, literal)) =>
        literal.value
      };

    let (loc, moduleBody) = m.body;
    let body = moduleBody.body;
    body |> List.map(handleStatement(~moduleName)) |> List.flatten;

  | Flow_parser.Ast.Statement.DeclareVariable(v) =>
    let (loc, varName) = v.id;

    [
      AstUtils.makeExtern(
        ~moduleName,
        ~defaultExport=false,
        ~externName=varName,
        ~externType=AstUtils.makeNamedType("int"),
      ),
    ];

  | Flow_parser.Ast.Statement.DeclareFunction(f) =>
    let (loc, functionName) = f.id;

    [
      AstUtils.makeExtern(
        ~moduleName,
        ~defaultExport=false,
        ~externName=functionName,
        ~externType=
          AstUtils.makeFunctionType2(
            [
              AstUtils.makeNamedType("int"),
              AstUtils.makeNamedType("string"),
            ],
            AstUtils.makeNamedType("int"),
          ),
      ),
    ];

  | _ => []
  };

let parse = file => {
  let (ast, errors) =
    Flow_parser.Parser_flow.program_file(file.source, None);

  if (List.length(errors) > 0) {
    raise(ParseErrors(errors));
  };

  let (_, statements, _) = ast;

  let program = statements |> List.map(handleStatement) |> List.flatten;

  Reason_toolchain.RE.print_implementation_with_comments(
    Format.str_formatter,
    (program, []),
  );

  let output = Format.flush_str_formatter();
  print_string(output);

  ();
};
