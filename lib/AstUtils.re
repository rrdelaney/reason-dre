open Ast_404;

let loc = Location.none;

let makeNamedType = typeName : Parsetree.core_type => {
  ptyp_desc:
    Parsetree.Ptyp_constr({txt: Longident.Lident(typeName), loc}, []),
  ptyp_loc: loc,
  ptyp_attributes: [],
};

let makeNamedTypeVar = typeName : Parsetree.core_type => {
  ptyp_desc: Parsetree.Ptyp_var(typeName),
  ptyp_loc: loc,
  ptyp_attributes: [],
};

let makeAppliedType = (typeName, args) : Parsetree.core_type => {
  ptyp_desc:
    Parsetree.Ptyp_constr({txt: Longident.Lident(typeName), loc}, args),
  ptyp_loc: loc,
  ptyp_attributes: [],
};

/** Takes a list of tuples: (label if optional, type) */
let makeFunctionType = (params, returnType) =>
  List.fold_right(
    ((name, paramType), t) =>
      Parsetree.{
        ptyp_desc:
          Parsetree.Ptyp_arrow(
            switch (name) {
            | Some(name) => Asttypes.Optional(name)
            | None => Asttypes.Nolabel
            },
            paramType,
            t,
          ),
        ptyp_loc: loc,
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

let makeBsScopeAttribute = (~namespaces) : Parsetree.attribute => (
  {txt: "bs.scope", loc},
  Parsetree.PStr([
    {
      pstr_desc:
        Parsetree.Pstr_eval(
          {
            pexp_desc:
              Parsetree.Pexp_tuple(
                List.map(
                  namespace =>
                    Parsetree.{
                      pexp_desc:
                        Parsetree.Pexp_constant(
                          Parsetree.Pconst_string(namespace, None),
                        ),
                      pexp_loc: loc,
                      pexp_attributes: [],
                    },
                  namespaces,
                ),
              ),
            pexp_loc: loc,
            pexp_attributes: [],
          },
          [],
        ),
      pstr_loc: loc,
    },
  ]),
);

let makeBsDerivingAttribute = () : Parsetree.attribute => (
  {txt: "bs.deriving", loc},
  Parsetree.PStr([
    {
      pstr_desc:
        Parsetree.Pstr_eval(
          {
            pexp_desc:
              Parsetree.Pexp_ident({txt: Longident.Lident("abstract"), loc}),
            pexp_loc: loc,
            pexp_attributes: [],
          },
          [],
        ),
      pstr_loc: loc,
    },
  ]),
);

let makeBsSendAttribute = () : Parsetree.attribute => (
  {txt: "bs.send", loc},
  Parsetree.PStr([]),
);

let makeBsValAttribute = () : Parsetree.attribute => (
  {txt: "bs.val", loc},
  Parsetree.PStr([]),
);

let makeBsNewAttribute = () : Parsetree.attribute => (
  {txt: "bs.new", loc},
  Parsetree.PStr([]),
);

let makeBsOptionalAttribute = () : Parsetree.attribute => (
  {txt: "bs.optional", loc},
  Parsetree.PStr([]),
);

let makeExtern =
    (
      ~moduleName,
      ~namespaces,
      ~defaultExport,
      ~bindingName,
      ~externName,
      ~externType,
    )
    : Parsetree.structure_item => {
  pstr_desc:
    Parsetree.Pstr_primitive({
      pval_name: {
        txt:
          switch (bindingName) {
          | Some(name) => name
          | None => externName
          },
        loc,
      },
      pval_type: externType,
      pval_prim: [
        switch (defaultExport, moduleName) {
        | (true, Some(name)) => name
        | _ => externName
        },
      ],
      pval_attributes:
        switch (moduleName, namespaces) {
        | (Some(name), _) => [
            makeBsModuleAttibute(~moduleName=name, ~defaultExport),
          ]
        | (_, Some(names)) => [
            makeBsScopeAttribute(~namespaces=names),
            makeBsValAttribute(),
          ]
        | (None, None) => [makeBsValAttribute()]
        },
      pval_loc: loc,
    }),
  pstr_loc: loc,
};

let makeIdentityExtern = (~externName, ~externType) : Parsetree.structure_item => {
  pstr_desc:
    Parsetree.Pstr_primitive({
      pval_name: {
        txt: externName,
        loc,
      },
      pval_type: externType,
      pval_prim: ["%identity"],
      pval_attributes: [],
      pval_loc: loc,
    }),
  pstr_loc: loc,
};

let makeNewExtern =
    (~moduleName, ~namespaces, ~localName, ~externName, ~externType)
    : Parsetree.structure_item => {
  pstr_desc:
    Parsetree.Pstr_primitive({
      pval_name: {
        txt: localName,
        loc,
      },
      pval_type: externType,
      pval_prim: [externName],
      pval_attributes:
        switch (moduleName, namespaces) {
        | (Some(name), _) => [
            makeBsModuleAttibute(~moduleName=name, ~defaultExport=false),
            makeBsNewAttribute(),
          ]
        | (_, Some(names)) => [
            makeBsScopeAttribute(~namespaces=names),
            makeBsValAttribute(),
            makeBsNewAttribute(),
          ]
        | (None, None) => [makeBsValAttribute(), makeBsNewAttribute()]
        },
      pval_loc: loc,
    }),
  pstr_loc: loc,
};

let makeMethodExtern = (~methodName, ~methodType) : Parsetree.structure_item => {
  pstr_desc:
    Parsetree.Pstr_primitive({
      pval_name: {
        txt: methodName,
        loc,
      },
      pval_type:
        Parsetree.{
          ptyp_desc:
            Parsetree.Ptyp_arrow(
              Asttypes.Nolabel,
              makeNamedType("t"),
              methodType,
            ),
          ptyp_loc: loc,
          ptyp_attributes: [],
        },
      pval_prim: [methodName],
      pval_attributes: [makeBsSendAttribute()],
      pval_loc: loc,
    }),
  pstr_loc: loc,
};

/** Takes a list of tuples: (name, optional, type) */
let makeInterfaceDeclaration =
    (~name, ~typeParamNames, ~fields)
    : Parsetree.structure_item => {
  pstr_desc:
    Parsetree.Pstr_type(
      Asttypes.Recursive,
      [
        {
          ptype_name: {
            txt: name,
            loc,
          },
          ptype_params:
            List.map(
              paramName => (makeNamedTypeVar(paramName), Asttypes.Invariant),
              typeParamNames,
            ),
          ptype_cstrs: [],
          ptype_kind:
            if (List.length(fields) != 0) {
              Parsetree.Ptype_record(
                List.map(
                  ((fieldName, isOptional, fieldType)) =>
                    Parsetree.{
                      pld_name: {
                        txt: fieldName,
                        loc,
                      },
                      pld_mutable: Asttypes.Immutable,
                      pld_type: fieldType,
                      pld_loc: loc,
                      pld_attributes:
                        if (isOptional) {
                          [makeBsOptionalAttribute()];
                        } else {
                          [];
                        },
                    },
                  fields,
                ),
              );
            } else {
              Parsetree.Ptype_abstract;
            },
          ptype_private: Asttypes.Public,
          ptype_manifest: None,
          ptype_attributes:
            if (List.length(fields) != 0) {
              [makeBsDerivingAttribute()];
            } else {
              [];
            },
          ptype_loc: loc,
        },
      ],
    ),
  pstr_loc: loc,
};

let makeBareType = (~typeName) : Parsetree.structure_item => {
  pstr_desc:
    Parsetree.Pstr_type(
      Asttypes.Recursive,
      [
        {
          ptype_name: {
            txt: typeName,
            loc,
          },
          ptype_params: [],
          ptype_cstrs: [],
          ptype_kind: Parsetree.Ptype_abstract,
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
