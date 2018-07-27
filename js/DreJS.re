let compile = (filename, source) => {
  let file =
    Lib.DreParser.{
      filename: Js.to_string(filename),
      source: Js.to_string(source),
    };

  let compiled = ref("");
  let errMsg = ref("");

  Lib.ErrorUtils.withBufferedError(() =>
    compiled := Lib.DreParser.parse(file)
  );
  errMsg := Lib.ErrorUtils.MsgBuf.flushDiagnostics();

  Js.array([|Js.string(compiled^), Js.string(errMsg^)|]);
};

Js.export("compile", compile);
