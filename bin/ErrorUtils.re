let printErrMsg = () => {
  print_endline(Chalk.red("There was a problem compiling your bindings :("));
  print_newline();
};

let formatErrorLoc = (~loc: Flow_parser.Loc.t, ~msg) => {
  let source =
    switch (loc.source) {
    | Some(Flow_parser.File_key.SourceFile(src)) => src
    | _ => ""
    };

  let sourceLines = Array.of_list(String.split_on_char('\n', source));
  let isSingleLineErr = loc.start.line == loc._end.line;

  let startLine = max(loc.start.line - 2, 0);
  let endLine = min(loc._end.line + 2, Array.length(sourceLines));
  let lineLen = endLine - startLine;

  let relevantLines = Array.sub(sourceLines, startLine, lineLen);

  let maxLineNumLen =
    relevantLines
    |> Array.mapi((i, _) => i + startLine + 1)
    |> Array.fold_left(
         (maxLen, lineNum) =>
           max(String.length(string_of_int(lineNum)), maxLen),
         0,
       );

  let padStr = s => s ++ String.make(maxLineNumLen - String.length(s), ' ');

  let linesWithNumber =
    Array.mapi(
      (i, line) =>
        Chalk.cyan(padStr(string_of_int(i + startLine + 1)) ++ " | ")
        ++ line,
      relevantLines,
    );

  Array.iteri(
    (i, line) =>
      if (isSingleLineErr && i + startLine + 1 == loc.start.line) {
        print_endline(Chalk.red("> ") ++ line);
        print_string(String.make(loc.start.column + 6, ' '));
        print_string(
          Chalk.red(String.make(loc._end.column - loc.start.column, '^')),
        );
        print_endline(Chalk.red(" " ++ msg));
      } else if (loc.start.line <= i
                 + startLine
                 + 1
                 && i
                 + startLine
                 + 1 <= loc._end.line) {
        print_endline("> " ++ line);
      } else {
        print_endline("  " ++ line);
      },
    linesWithNumber,
  );

  print_newline();
};

let formatHint = (~msg) => print_endline("  " ++ msg);

let formatError = fn =>
  try (ignore(fn())) {
  | Flow_parser.Parse_error.Error(errs)
  | Lib.DreParser.ParseError(errs) =>
    printErrMsg();
    List.iter(
      ((loc, err)) =>
        formatErrorLoc(~loc, ~msg=Flow_parser.Parse_error.PP.error(err)),
      errs,
    );

  | Lib.DreParser.TypeAliasNameMustBeLowercase(_name, loc) =>
    printErrMsg();
    formatErrorLoc(~loc, ~msg="Type alias names must be lowercase");

  | Lib.DreParser.InterfaceNameMustBeUppercase(_name, loc) =>
    printErrMsg();
    formatErrorLoc(~loc, ~msg="Interface names must be uppercase");

  | Lib.TypeUtils.TypeNotSupported(loc) =>
    printErrMsg();
    formatErrorLoc(~loc, ~msg="This type isn't supported yet, sorry!");

  | Lib.TypeUtils.TypeNotInScope(typeName, loc) =>
    printErrMsg();
    formatErrorLoc(
      ~loc,
      ~msg="Could not find type \"" ++ typeName ++ "\" in scope",
    );

    switch (typeName) {
    | "int" => formatHint(~msg="Did you mean \"number\"?")
    | _ => ()
    };

  | Lib.DreParser.ModuleNameMustBeStringLiteral(loc) => ()
  | Lib.TypeUtils.ObjectFieldNotSupported(loc) => ()
  };
