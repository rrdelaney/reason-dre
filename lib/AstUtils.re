open Ast_404;

let loc = Location.none;

let makeNamedType = typeName : Parsetree.core_type => {
  ptyp_desc:
    Parsetree.Ptyp_constr({txt: Longident.Lident(typeName), loc}, []),
  ptyp_loc: loc,
  ptyp_attributes: [],
};

let makeFunctionType = (params, returnType) =>
  List.fold_right(
    (paramType, t) =>
      Parsetree.{
        ptyp_desc: Parsetree.Ptyp_arrow(Asttypes.Nolabel, paramType, t),
        ptyp_loc: Location.none,
        ptyp_attributes: [],
      },
    params,
    returnType,
  );

let makeObjectType = fields : Parsetree.core_type => {
  ptyp_desc:
    Parsetree.Ptyp_constr(
      {txt: Longident.Lident("Js.t"), loc},
      [
        {
          ptyp_desc:
            Parsetree.Ptyp_object(
              List.map(
                ((fieldName, fieldType)) => (fieldName, [], fieldType),
                fields,
              ),
              Asttypes.Closed,
            ),
          ptyp_loc: loc,
          ptyp_attributes: [],
        },
      ],
    ),
  ptyp_loc: loc,
  ptyp_attributes: [],
};

let makeBsModuleAttibute = (~moduleName, ~defaultExport) : Parsetree.attribute => (
  {txt: "bs.module", loc},
  if (defaultExport) {
    Parsetree.PStr([]);
  } else {
    Parsetree.PStr([
      {
        pstr_desc:
          Parsetree.Pstr_eval(
            {
              pexp_desc:
                Parsetree.Pexp_constant(
                  Parsetree.Pconst_string(moduleName, None),
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
    : Parsetree.structure_item => {
  pstr_desc:
    Parsetree.Pstr_primitive({
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

let makeInterfaceDeclaration = (~name, ~fields) : Parsetree.structure_item => {
  pstr_desc:
    Parsetree.Pstr_type(
      Asttypes.Recursive,
      [
        {
          ptype_name: {
            txt: name,
            loc,
          },
          ptype_params: [],
          ptype_cstrs: [],
          ptype_kind:
            Parsetree.Ptype_record(
              List.map(
                ((fieldName, fieldType)) =>
                  Parsetree.{
                    pld_name: {
                      txt: fieldName,
                      loc,
                    },
                    pld_mutable: Asttypes.Immutable,
                    pld_type: fieldType,
                    pld_loc: loc,
                    pld_attributes: [],
                  },
                fields,
              ),
            ),
          ptype_private: Asttypes.Public,
          ptype_manifest: None,
          ptype_attributes: [],
          ptype_loc: loc,
        },
      ],
    ),
  pstr_loc: loc,
};

let makeTypeDeclaration = (~aliasName, ~aliasType) : Parsetree.structure_item => {
  pstr_desc:
    Parsetree.Pstr_type(
      Asttypes.Recursive,
      [
        {
          ptype_name: {
            txt: aliasName,
            loc,
          },
          ptype_params: [],
          ptype_cstrs: [],
          ptype_kind: Parsetree.Ptype_abstract,
          ptype_private: Asttypes.Public,
          ptype_manifest: Some(aliasType),
          ptype_attributes: [],
          ptype_loc: loc,
        },
      ],
    ),
  pstr_loc: loc,
};

let makeModule = (moduleName, moduleItems) : Parsetree.structure_item => {
  pstr_desc:
    Parsetree.Pstr_module({
      pmb_name: {
        txt: moduleName,
        loc,
      },
      pmb_expr: {
        pmod_desc: Parsetree.Pmod_structure(moduleItems),
        pmod_loc: loc,
        pmod_attributes: [],
      },
      pmb_attributes: [],
      pmb_loc: loc,
    }),
  pstr_loc: loc,
};
