type scopeType =
  | TypeVariable(string)
  | Named(string);

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

let withName = (scopeName, scope) => {
  ...scope,
  scopeName: Some(scopeName),
  types: [Named(scopeName), ...scope.types],
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
    | Named(name) => t == name,
    scope.types,
  );

let get = (t, scope) =>
  scope.types
  |> List.filter(
       fun
       | TypeVariable(name) => t == name
       | Named(name) => t == name,
     )
  |> (
    fun
    | [t, ..._] => Some(t)
    | [] => None
  );
