module IO = {
  open Lib.DreParser;

  let readFile = filename => {
    let inChannel = open_in(filename);
    let fileLength = in_channel_length(inChannel);
    let outputString = Bytes.create(fileLength);
    really_input(inChannel, outputString, 0, fileLength);
    close_in(inChannel);
    let source = Bytes.to_string(outputString);

    {source, filename};
  };

  let writeFile = file => {
    let {source, filename} = file;
    let outChannel = open_out(filename);
    output_string(outChannel, source);
    close_out(outChannel);
  };
};

let dreFilenameToReFilename = fname =>
  String.sub(fname, 0, String.length(fname) - 3) ++ "re";

let printHelp = () => {
  print_endline("Dre Compiler");
  print_endline("  Usage: dre.exe [...files]");
};

let compileFile = (~stdout, fname) => {
  let input = IO.readFile(fname);
  let output = Lib.DreParser.parse(input);
  let outputFname = dreFilenameToReFilename(fname);

  if (stdout) {
    print_endline(output);
  } else {
    IO.writeFile({filename: outputFname, source: output});
  };
};

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
      Lib.ErrorUtils.withBufferedError(() =>
        compileFile(~stdout=args.stdout, f)
      );
      Lib.ErrorUtils.MsgBuf.flush() |> print_string;
    },
    args.files,
  );
};
