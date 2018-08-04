module MsgBuf = {
  type message =
    | Endline
    | Diagnostic(string)
    | Str(string)
    | Red(string)
    | Cyan(string);

  let buffer = ref([]);

  let push = msgs => buffer := List.concat([List.rev(msgs), buffer^]);

  let flush = () => {
    let flushedStr =
      List.fold_right(
        (msg, buf) =>
          buf
          ++ (
            switch (msg) {
            | Str(str) => str
            | Red(str) => Chalk.red(str)
            | Cyan(str) => Chalk.cyan(str)
            | Endline => "\n"
            | Diagnostic(str) => ""
            }
          ),
        buffer^,
        "",
      );

    buffer := [];
    flushedStr;
  };

  let flushPlainStr = () => {
    let flushedStr =
      List.fold_right(
        (msg, buf) =>
          buf
          ++ (
            switch (msg) {
            | Str(str)
            | Red(str)
            | Cyan(str) => str
            | Endline => "\n"
            | Diagnostic(str) => ""
            }
          ),
        buffer^,
        "",
      );

    buffer := [];
    flushedStr;
  };

  let flushDiagnostics = () => {
    let flushedStr =
      List.fold_right(
        (msg, buf) =>
          buf
          ++ (
            switch (msg) {
            | Str(_)
            | Red(_)
            | Cyan(_)
            | Endline => ""
            | Diagnostic(str) => str
            }
          ),
        buffer^,
        "",
      );

    buffer := [];
    flushedStr;
  };
};

let bufferErrMsg = () =>
  MsgBuf.push([
    MsgBuf.Red("There was a problem compiling your bindings :("),
    MsgBuf.Endline,
  ]);

let bufferErrorLoc = (~loc: Flow_parser.Loc.t, ~msg) => {
  MsgBuf.push([
    MsgBuf.Diagnostic(
      string_of_int(loc.start.line)
      ++ ":"
      ++ string_of_int(loc.start.column)
      ++ ":"
      ++ string_of_int(loc._end.line)
      ++ ":"
      ++ string_of_int(loc._end.column)
      ++ ":"
      ++ msg,
    ),
  ]);

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
      (i, line) => (
        MsgBuf.Cyan(padStr(string_of_int(i + startLine + 1)) ++ " | "),
        MsgBuf.Str(line),
      ),
      relevantLines,
    );

  Array.iteri(
    (i, (lineNum, line)) =>
      if (isSingleLineErr && i + startLine + 1 == loc.start.line) {
        MsgBuf.push([
          MsgBuf.Red("> "),
          lineNum,
          line,
          MsgBuf.Endline,
          MsgBuf.Str(String.make(loc.start.column + 5 + maxLineNumLen, ' ')),
          MsgBuf.Red(String.make(loc._end.column - loc.start.column, '^')),
          MsgBuf.Red(" " ++ msg),
          MsgBuf.Endline,
        ]);
      } else if (loc.start.line <= i
                 + startLine
                 + 1
                 && i
                 + startLine
                 + 1 <= loc._end.line) {
        MsgBuf.push([MsgBuf.Str("> "), lineNum, line, MsgBuf.Endline]);
      } else {
        MsgBuf.push([MsgBuf.Str("  "), lineNum, line, MsgBuf.Endline]);
      },
    linesWithNumber,
  );

  MsgBuf.push([MsgBuf.Endline]);
};

let bufferHint = (~msg) => {
  MsgBuf.push([MsgBuf.Diagnostic("\n" ++ msg)]);
  MsgBuf.push([MsgBuf.Str("  " ++ msg)]);
};

let withBufferedError = fn =>
  try (ignore(fn())) {
  | Flow_parser.Parse_error.Error(errs)
  | DreParser.ParseError(errs) =>
    bufferErrMsg();
    List.iter(
      ((loc, err)) =>
        bufferErrorLoc(~loc, ~msg=Flow_parser.Parse_error.PP.error(err)),
      errs,
    );

  | DreParser.TypeAliasNameMustBeLowercase(_name, loc) =>
    bufferErrMsg();
    bufferErrorLoc(~loc, ~msg="Type alias names must be lowercase");

  | DreParser.InterfaceNameMustBeUppercase(_name, loc) =>
    bufferErrMsg();
    bufferErrorLoc(~loc, ~msg="Interface names must be uppercase");

  | DreParser.ClassNameMustBeUppercase(_name, loc) =>
    bufferErrMsg();
    bufferErrorLoc(~loc, ~msg="Class names must be uppercase");

  | TypeUtils.TypeNotSupported(loc) =>
    bufferErrMsg();
    bufferErrorLoc(~loc, ~msg="This type isn't supported yet, sorry!");

  | TypeUtils.TypeNotInScope(typeName, loc) =>
    bufferErrMsg();
    bufferErrorLoc(
      ~loc,
      ~msg="Could not find type \"" ++ typeName ++ "\" in scope",
    );

    switch (typeName) {
    | "number" => bufferHint(~msg="Did you mean \"float\"?")
    | "void" => bufferHint(~msg="Did you mean \"unit\"?")
    | "Promise" => bufferHint(~msg="Did you mean \"Js.Promise\"?")
    | _ => ()
    };

  | TypeUtils.TypeVarsMustBeLowercase(_name, loc) =>
    bufferErrMsg();
    bufferErrorLoc(~loc, ~msg="Type variable names must be lowercase");

  | TypeUtils.NotEnoughTypeArguments(typeName, expected, applied, loc) =>
    bufferErrMsg();
    bufferErrorLoc(
      ~loc,
      ~msg=
        typeName
        ++ " expected "
        ++ string_of_int(expected)
        ++ " type arguments but here it was applied with "
        ++ string_of_int(applied),
    );

  | TypeUtils.ObjectFieldNotSupported(loc) =>
    bufferErrMsg();
    bufferErrorLoc(
      ~loc,
      ~msg="This type of object field isn't supported yet",
    );

  | DreParser.ModuleExportsMustBeInModule(loc) =>
    bufferErrMsg();
    bufferErrorLoc(
      ~loc,
      ~msg="module.exports can only be declared inside of a module!",
    );
    bufferHint(~msg="Did you mean to wrap this in a \"declare module\"?");

  | DreParser.ModuleNameMustBeStringLiteral(loc) => ()
  };
