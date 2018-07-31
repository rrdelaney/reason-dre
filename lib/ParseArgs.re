type args = {
  help: bool,
  stdout: bool,
  files: list(string),
};

let defaultArgs = {help: false, stdout: false, files: []};

let parse = (argsList: array(string)) : args =>
  argsList
  |> Array.to_list
  |> List.fold_left(
       (args, param) =>
         switch (param) {
         | "--help" => {...args, help: true}
         | "--stdout" => {...args, stdout: true}
         | s => {...args, files: [s, ...args.files]}
         },
       defaultArgs,
     );
