exception
  ParseErrors(list((Flow_parser.Loc.t, Flow_parser.Parser_common.Error.t)));
exception ModuleNameMustBeStringLiteral(Flow_parser.Loc.t);

type file = {
  source: string,
  filename: string,
};

let rec handleStatement = ((loc, statement)) =>
  switch (statement) {
  | Flow_parser.Ast.Statement.DeclareModule(m) =>
    let moduleName =
      switch (m.id) {
      | Flow_parser.Ast.Statement.DeclareModule.Identifier(_) =>
        raise(ModuleNameMustBeStringLiteral(loc))
      | Flow_parser.Ast.Statement.DeclareModule.Literal((loc, literal)) =>
        literal.value
      };

    print_endline("DECLARED MODULE " ++ moduleName);

    let (loc, moduleBody) = m.body;
    let body = moduleBody.body;
    let _ = List.map(handleStatement, body);
    ();

  | Flow_parser.Ast.Statement.DeclareFunction(f) =>
    let (loc, functionName) = f.id;
    print_endline("DECLARED FUNCTION " ++ functionName);

  | _ => ()
  };

let parse = file => {
  let (ast, errors) =
    Flow_parser.Parser_flow.program_file(file.source, None);

  if (List.length(errors) > 0) {
    raise(ParseErrors(errors));
  };

  let (_, statements, _) = ast;

  let _ = List.map(handleStatement, statements);

  let externalDecl: Ast_404.Parsetree.structure_item = {
    pstr_desc:
      Ast_404.Parsetree.Pstr_primitive({
        pval_name: {
          txt: "thing",
          loc: Ast_404.Location.none,
        },
        pval_type: {
          ptyp_desc:
            Ast_404.Parsetree.Ptyp_constr(
              {
                txt: Ast_404.Longident.Lident("int"),
                loc: Ast_404.Location.none,
              },
              [],
            ),
          ptyp_loc: Ast_404.Location.none,
          ptyp_attributes: [],
        },
        pval_prim: ["thing"],
        pval_attributes: [
          (
            {txt: "bs.module", loc: Ast_404.Location.none},
            Ast_404.Parsetree.PStr([]),
          ),
        ],
        pval_loc: Ast_404.Location.none,
      }),
    pstr_loc: Ast_404.Location.none,
  };

  let program = [externalDecl];

  Reason_toolchain.RE.print_implementation_with_comments(
    Format.str_formatter,
    (program, []),
  );

  let output = Format.flush_str_formatter();
  print_string(output);

  ();
};
