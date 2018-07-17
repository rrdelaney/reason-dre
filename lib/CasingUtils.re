let getFirstLetter = s => String.sub(s, 0, 1);

let isFirstLetterUppercase = s =>
  String.uppercase_ascii(getFirstLetter(s)) == getFirstLetter(s);

let isFirstLetterLowercase = s =>
  String.lowercase_ascii(getFirstLetter(s)) == getFirstLetter(s);
