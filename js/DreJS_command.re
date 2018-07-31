module IO = {
  open Lib.DreParser;

  let readFile = filename => {
    let filenameJS = Js.string(filename);
    let impl = Js.Unsafe.js_expr("require('fs').readFileSync");
    let buffer = Js.Unsafe.fun_call(impl, [|Js.Unsafe.inject(filenameJS)|]);
    let sourceJS = Js.Unsafe.meth_call(buffer, "toString", [||]);
    let source = Js.to_string(sourceJS);

    {source, filename};
  };

  let writeFile = file => {
    let {source, filename} = file;
    let sourceJS = Js.string(source);
    let filenameJS = Js.string(filename);
    let impl = Js.Unsafe.js_expr("require('fs').writeFileSync");
    let _ =
      Js.Unsafe.fun_call(
        impl,
        [|Js.Unsafe.inject(filenameJS), Js.Unsafe.inject(sourceJS)|],
      );

    ();
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
let args = Lib.ParseArgs.parse(argv);

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
