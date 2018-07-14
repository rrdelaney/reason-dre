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

let argv = Array.sub(Sys.argv, 1, Array.length(Sys.argv) - 1);
let args = ParseArgs.parse(argv);

if (List.length(args.files) == 0) {
  print_endline("Must provide a filename!");
  exit(1);
};

List.iter(
  fname => {
    let input = IO.readFile(fname);
    let _output = Lib.DreParser.parse(input);
    ();
  },
  args.files,
);
