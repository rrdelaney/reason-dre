let loc = Ast_404.Location.none;

let makeNamedType = typeName : Ast_404.Parsetree.core_type => {
  ptyp_desc:
    Ast_404.Parsetree.Ptyp_constr(
      {txt: Ast_404.Longident.Lident(typeName), loc},
      [],
    ),
  ptyp_loc: loc,
  ptyp_attributes: [],
};

let makeFunctionType = (args, _returnType) : Ast_404.Parsetree.core_type => {
  ptyp_desc:
    Ast_404.Parsetree.Ptyp_arrow(
      Ast_404.Asttypes.Nolabel,
      {
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
      {
        ptyp_desc:
          Ast_404.Parsetree.Ptyp_arrow(
            Ast_404.Asttypes.Nolabel,
            {
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
            {
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
          ),
        ptyp_loc: Ast_404.Location.none,
        ptyp_attributes: [],
      },
    ),
  ptyp_loc: Ast_404.Location.none,
  ptyp_attributes: [],
};

let makeBsModuleAttibute =
    (~moduleName, ~defaultExport)
    : Ast_404.Parsetree.attribute => (
  {txt: "bs.module", loc},
  if (defaultExport) {
    Ast_404.Parsetree.PStr([]);
  } else {
    Ast_404.Parsetree.PStr([
      {
        pstr_desc:
          Ast_404.Parsetree.Pstr_eval(
            {
              pexp_desc:
                Ast_404.Parsetree.Pexp_constant(
                  Ast_404.Parsetree.Pconst_string(moduleName, None),
                ),
              pexp_loc: loc,
              pexp_attributes: [],
            },
            [],
          ),
        pstr_loc: loc,
      },
    ]);
  },
);

let makeExtern =
    (~moduleName, ~defaultExport, ~externName, ~externType)
    : Ast_404.Parsetree.structure_item => {
  pstr_desc:
    Ast_404.Parsetree.Pstr_primitive({
      pval_name: {
        txt: externName,
        loc,
      },
      pval_type: externType,
      pval_prim: [externName],
      pval_attributes: [makeBsModuleAttibute(~moduleName, ~defaultExport)],
      pval_loc: loc,
    }),
  pstr_loc: loc,
};
