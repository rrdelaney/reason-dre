type scopeType =
  | TypeVariable(string)
  | Named(string);

type scope = {
  name: string,
  mutable types: list(scopeType),
};

let push = (t, scope) => scope.types = [t, ...scope.types];

let clone = scope => {name: scope.name, types: scope.types};

let make = () => {name: "", types: []};

let makeNamed = name => {name, types: [Named(name)]};

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
