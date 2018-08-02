module type IO = {
  let readFile: string => DreParser.file;
  let writeFile: DreParser.file => unit;
};

let dreFilenameToReFilename = fname =>
  String.sub(fname, 0, String.length(fname) - 3) ++ "re";

let printHelp = () => {
  print_endline("Dre Compiler");
  print_endline("  Usage: dre.exe [...files]");
};

module Make = (IO: IO) => {
  let compileFile = (~stdout, fname) => {
    let input = IO.readFile(fname);
    let output = DreParser.parse(input);
    let outputFname = dreFilenameToReFilename(fname);

    if (stdout) {
      print_endline(output);
    } else {
      IO.writeFile({filename: outputFname, source: output});
    };
  };

  let run = () => {
    let argv = Array.sub(Sys.argv, 1, Array.length(Sys.argv) - 1);
    let args = ParseArgs.parse(argv);

    if (List.length(args.files) == 0 && ! args.help) {
      print_endline("Must provide at least one file");
      exit(1);
    } else if (args.help) {
      printHelp();
    } else {
      List.iter(
        f => {
          ErrorUtils.withBufferedError(() =>
            compileFile(~stdout=args.stdout, f)
          );
          ErrorUtils.MsgBuf.flush() |> print_string;
        },
        args.files,
      );
    };
  };
};
