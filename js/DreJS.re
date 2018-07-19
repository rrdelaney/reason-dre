let compile = (filename, source) => {
  let file =
    Lib.DreParser.{
      filename: Js.to_string(filename),
      source: Js.to_string(source),
    };

  Js.string(Lib.DreParser.parse(file));
};

Js.export("compile", compile);
