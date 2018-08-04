let getFirstLetter = s => String.sub(s, 0, 1);

let isFirstLetterUppercase = s =>
  String.uppercase_ascii(getFirstLetter(s)) == getFirstLetter(s);

let isFirstLetterLowercase = s =>
  String.lowercase_ascii(getFirstLetter(s)) == getFirstLetter(s);

let makeVariableName = s => {
  let buf = ref("");

  String.iteri((i, ch) => {
    let prevChar =
      try (s.[i - 1]) {
      | Invalid_argument(_) => '0'
      };
    ();
  });
};
