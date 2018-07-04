exception ParseErrors(list((Loc.t, Parser_common.Error.t)));

type file = {
  source: string,
  filename: string,
};

let parse = file => {
  let (ast, errors) = Parser_flow.program_file(file.source, None);

  if (List.length(errors) > 0) {
    raise(ParseErrors(errors));
  };

  let (_, statements, _) = ast;
  ();
};
