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

type scopeType =
  | TypeVariable(string)
  | TypeAlias(typeAlias)
  | Interface(interface)
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
    | TypeVariable(name) => t == name
    | Interface({name}) => t == name
    | TypeAlias({name}) => t == name
    | BuiltIn({dreName}) => t == dreName,
    scope.types,
  );

let get = (t, scope) =>
  scope.types
  |> List.filter(
       fun
       | TypeVariable(name) => t == name
       | Interface({name}) => t == name
       | TypeAlias({name}) => t == name
       | BuiltIn({dreName}) => t == dreName,
     )
  |> (
    fun
    | [t, ..._] => Some(t)
    | [] => None
  );
