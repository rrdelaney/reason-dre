let getFirstLetter = s => String.sub(s, 0, 1);

let isFirstLetterUppercase = s =>
  String.uppercase_ascii(getFirstLetter(s)) == getFirstLetter(s);

let isFirstLetterLowercase = s =>
  String.lowercase_ascii(getFirstLetter(s)) == getFirstLetter(s);

let makeVariableName = s => {
  let buf = ref("");
  let append = c => buf := String.concat("", [buf^, Char.escaped(c)]);

  String.iteri(
    (i, ch) => {
      let prevChar =
        try (s.[i - 1]) {
        | Invalid_argument(_) => '0'
        };

      switch (prevChar, ch) {
      | ('-', c) => append(Char.uppercase_ascii(c))
      | ('0', c) => append(Char.lowercase_ascii(c))
      | (_, '-') => ()
      | (_, c) => append(c)
      };
    },
    s,
  );

  buf^;
};

let makeModuleName = s => {
  let s = makeVariableName(s);
  let head = String.uppercase_ascii(getFirstLetter(s));
  let tail = String.sub(s, 1, String.length(s) - 1);
  String.concat("", [head, tail]);
};
