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

  | Flow_parser.Ast.Statement.DeclareFunction(f) =>
    let (loc, functionName) = f.id;

    [
      {
        pstr_desc:
          Ast_404.Parsetree.Pstr_primitive({
            pval_name: {
              txt: functionName,
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
            pval_prim: [functionName],
            pval_attributes: [
              (
                {txt: "bs.module", loc: Ast_404.Location.none},
                Ast_404.Parsetree.PStr([
                  {
                    pstr_desc:
                      Ast_404.Parsetree.Pstr_eval(
                        {
                          pexp_desc:
                            Ast_404.Parsetree.Pexp_constant(
                              Ast_404.Parsetree.Pconst_string("mod", None),
                            ),
                          pexp_loc: Ast_404.Location.none,
                          pexp_attributes: [],
                        },
                        [],
                      ),
                    pstr_loc: Ast_404.Location.none,
                  },
                ]),
              ),
            ],
            pval_loc: Ast_404.Location.none,
          }),
        pstr_loc: Ast_404.Location.none,
      },
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
