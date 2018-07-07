exception ParseErrors(list((Loc.t, Parser_common.Error.t)));
exception ModuleNameMustBeStringLiteral(Loc.t);

type file = {
  source: string,
  filename: string,
};

let rec handleStatement = ((loc, statement)) =>
  switch (statement) {
  | Ast.Statement.DeclareModule(m) =>
    let moduleName =
      switch (m.id) {
      | Ast.Statement.DeclareModule.Identifier(_) =>
        raise(ModuleNameMustBeStringLiteral(loc))
      | Ast.Statement.DeclareModule.Literal((loc, literal)) => literal.value
      };

    print_endline("DECLARED MODULE " ++ moduleName);

    let (loc, moduleBody) = m.body;
    let body = moduleBody.body;
    let _ = List.map(handleStatement, body);
    ();

  | Ast.Statement.DeclareFunction(f) =>
    let (loc, functionName) = f.id;
    print_endline("DECLARED FUNCTION " ++ functionName);

  | _ => ()
  };

let parse = file => {
  let (ast, errors) = Parser_flow.program_file(file.source, None);

  if (List.length(errors) > 0) {
    raise(ParseErrors(errors));
  };

  let (_, statements, _) = ast;

  List.map(handleStatement, statements);
};
