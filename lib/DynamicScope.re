type builtIn = {
  dreName: string,
  reasonName: string,
};

type interface = {
  name: string,
  typeParamCount: int,
};

type typeAlias = {
  name: string,
  typeParamCount: int,
};

type classDef = {
  name: string,
  make: string => Ast_404.Parsetree.structure_item,
  typeParamCount: int,
};

type type_ =
  | TypeVariable(string)
  | TypeAlias(typeAlias)
  | Interface(interface)
  | ClassDef(classDef)
  | BuiltIn(builtIn);

type module_ =
  | NodeModule(string)
  | Namespace(string)
  | Declaration(string);

type scope = {
  modules: list(module_),
  mutable types: list(type_),
};

let make = () => {modules: [], types: []};

let clone = scope => {...scope, types: scope.types};

let withNodeModule = (moduleName, scope) => {
  ...scope,
  modules: [NodeModule(moduleName), ...scope.modules],
};

let withNamespace = (namespaceName, scope) => {
  ...scope,
  modules: [Namespace(namespaceName), ...scope.modules],
};

let withInterface = (~name, ~typeParamCount, scope) => {
  modules: [Declaration(name), ...scope.modules],
  types: [Interface({name, typeParamCount}), ...scope.types],
};

let is = (scopeName, scope) =>
  switch (scope.modules) {
  | [Declaration(s), ..._] when s == scopeName => true
  | _ => false
  };

let push = (t, scope) => scope.types = [t, ...scope.types];

let has = (t, scope) =>
  List.exists(
    fun
    | TypeVariable(name)
    | Interface({name})
    | ClassDef({name})
    | TypeAlias({name})
    | BuiltIn({dreName: name}) => t == name,
    scope.types,
  );

let get = (t, scope) =>
  scope.types
  |> List.filter(
       fun
       | TypeVariable(name)
       | Interface({name})
       | TypeAlias({name})
       | ClassDef({name})
       | BuiltIn({dreName: name}) => t == name,
     )
  |> (
    fun
    | [t, ..._] => Some(t)
    | [] => None
  );

let moduleName = scope =>
  List.fold_left(
    (maybeModuleName, module_) =>
      switch (module_) {
      | NodeModule(s) => Some(s)
      | Namespace(_)
      | Declaration(_) => maybeModuleName
      },
    None,
    scope.modules,
  );

let namespaces = scope => {
  let namespaces =
    List.fold_right(
      (module_, namespaces) =>
        switch (module_) {
        | Namespace(name) => [name, ...namespaces]
        | NodeModule(_)
        | Declaration(_) => namespaces
        },
      scope.modules,
      [],
    );

  if (List.length(namespaces) > 0) {
    Some(namespaces);
  } else {
    None;
  };
};
