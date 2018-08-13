type node('t) = {
  value: 't,
  children: list(node('t)),
};

let rec diff = (a, b) =>
  if (a.value != b.value) {
    Some(b.value);
  } else {
    try (
      List.fold_left2(
        (foundDiff, aChild, bChild) =>
          switch (foundDiff) {
          | Some(_) => foundDiff
          | _ => diff(aChild, bChild)
          },
        None,
        a.children,
        b.children,
      )
    ) {
    | Invalid_argument(_) => None
    };
  };
