let readFile = filename => {
  let inChannel = open_in(filename);
  let fileLength = in_channel_length(inChannel);
  let outputString = String.create(fileLength, inChannel);
  really_input(inChannel, outputString, 0, fileLength);
  close_in(inChannel);
  let source = Bytes.to_string(outputString);

  Lib.DreParser.{source, filename};
};
