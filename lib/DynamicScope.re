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

type scopeType =
  | TypeVariable(string)
  | TypeAlias(typeAlias)
  | Interface(interface)
  | ClassDef(classDef)
  | BuiltIn(builtIn);

type scope = {
  moduleName: option(string),
  scopeName: option(string),
  mutable types: list(scopeType),
};

let make = () => {moduleName: None, scopeName: None, types: []};

let clone = scope => {...scope, types: scope.types};

let withModule = (moduleName, scope) => {
  ...scope,
  moduleName: Some(moduleName),
};

let withInterface = (~name, ~typeParamCount, scope) => {
  ...scope,
  scopeName: Some(name),
  types: [Interface({name, typeParamCount}), ...scope.types],
};

let is = (scopeName, scope) =>
  switch (scope.scopeName) {
  | Some(s) => s == scopeName
  | None => false
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
