exception TypeNotSupported(Flow_parser.Loc.t);

let rec convertType =
        ((loc, t): Flow_parser.Ast.Type.t(Flow_parser.Loc.t))
        : Ast_404.Parsetree.core_type =>
  switch (t) {
  | Number => AstUtils.makeNamedType("float")
  | String => AstUtils.makeNamedType("string")
  | Boolean => AstUtils.makeNamedType("boolean")

  | Flow_parser.Ast.Type.Function(f) =>
    let (_loc, paramTypes) = f.params;
    let (retLoc, returnType) = f.return;
    let concreteParams = paramTypes.params;
    let concreteParamTypes =
      List.map(
        ((loc, param)) => Flow_parser.Ast.Type.Function.Param.(param.annot),
        concreteParams,
      );

    AstUtils.makeFunctionType(
      List.map(convertType, concreteParamTypes),
      convertType((retLoc, returnType)),
    );

  | Any => raise(TypeNotSupported(loc))
  | Mixed => raise(TypeNotSupported(loc))
  | Empty => raise(TypeNotSupported(loc))
  | Void => raise(TypeNotSupported(loc))
  | Null => raise(TypeNotSupported(loc))
  | Nullable(tt) => raise(TypeNotSupported(loc))
  | Object(tt) => raise(TypeNotSupported(loc))
  | Interface(tt) => raise(TypeNotSupported(loc))
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
