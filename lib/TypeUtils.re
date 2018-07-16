open Flow_parser;
open Ast_404;

exception TypeNotSupported(Loc.t);
exception ObjectFieldNotSupported(Loc.t);

let rec convertType = ((loc, t): Ast.Type.t(Loc.t)) : Parsetree.core_type =>
  switch (t) {
  | Number => AstUtils.makeNamedType("float")
  | String => AstUtils.makeNamedType("string")
  | Boolean => AstUtils.makeNamedType("boolean")

  | Ast.Type.Function(f) =>
    let (_loc, paramTypes) = f.params;
    let (retLoc, returnType) = f.return;
    let concreteParams = paramTypes.params;
    let concreteParamTypes =
      List.map(
        ((loc, param)) => Ast.Type.Function.Param.(param.annot),
        concreteParams,
      );

    AstUtils.makeFunctionType(
      List.map(convertType, concreteParamTypes),
      convertType((retLoc, returnType)),
    );

  | Object(tt) =>
    let objProps =
      Ast.Type.Object.(
        List.map(
          fun
          | Property((loc, prop)) => {
              open Ast.Type.Object.Property;
              let {key, value} = prop;

              let propName =
                switch (key) {
                | Ast.Expression.Object.Property.Identifier((loc, id)) => id

                | Ast.Expression.Object.Property.Literal((loc, _))
                | Ast.Expression.Object.Property.PrivateName((loc, _))
                | Ast.Expression.Object.Property.Computed((loc, _)) =>
                  raise(ObjectFieldNotSupported(loc))
                };
              let propType =
                switch (value) {
                | Init(t) => convertType(t)
                | Get((loc, _))
                | Set((loc, _)) => raise(ObjectFieldNotSupported(loc))
                };

              (propName, propType);
            }

          | SpreadProperty((loc, _))
          | Indexer((loc, _))
          | CallProperty((loc, _))
          | InternalSlot((loc, _)) => raise(ObjectFieldNotSupported(loc)),
          tt.properties,
        )
      );

    AstUtils.makeObjectType(objProps);

  | Interface(tt) => raise(TypeNotSupported(loc))

  | Any => raise(TypeNotSupported(loc))
  | Mixed => raise(TypeNotSupported(loc))
  | Empty => raise(TypeNotSupported(loc))
  | Void => raise(TypeNotSupported(loc))
  | Null => raise(TypeNotSupported(loc))
  | Nullable(tt) => raise(TypeNotSupported(loc))
  | Array(tt) => raise(TypeNotSupported(loc))
  | Generic(tt) => raise(TypeNotSupported(loc))
  | Union(a, b, c) => raise(TypeNotSupported(loc))
  | Intersection(a, b, c) => raise(TypeNotSupported(loc))
  | Typeof(tt) => raise(TypeNotSupported(loc))
  | Tuple(tt) => raise(TypeNotSupported(loc))
  | StringLiteral(tt) => raise(TypeNotSupported(loc))
  | NumberLiteral(tt) => raise(TypeNotSupported(loc))
  | BooleanLiteral(tt) => raise(TypeNotSupported(loc))
  | Exists => raise(TypeNotSupported(loc))
  };
