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

module CLI = Lib.DreCLI.Make(IO);

CLI.run();
