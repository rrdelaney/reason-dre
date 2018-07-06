let readFile = filename => {
  let inChannel = open_in(filename);
  let fileLength = in_channel_length(inChannel);
  let outputString = Bytes.create(fileLength);
  really_input(inChannel, outputString, 0, fileLength);
  close_in(inChannel);
  let source = Bytes.to_string(outputString);

  Lib.DreParser.{source, filename};
};

if (Array.length(Sys.argv) < 2) {
  print_endline("Must provide a filename!");
  exit(1);
};

let inputFilename = Sys.argv[1];
let input = readFile(inputFilename);
let output = Lib.DreParser.parse(input);
