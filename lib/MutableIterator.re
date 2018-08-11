type iter('t) = {
  replace: (int, 't) => unit,
  peek: int => 't,
};

let iterate = (f, t) => {
  let nodes = Array.of_list(t);
  let replace = (~offset, n, value) => nodes[offset + n + 1] = value;
  let peek = (~offset, n) => nodes[offset + n + 1];

  Array.iteri(
    (i, node) => {
      let iter = {replace: replace(~offset=i), peek: peek(~offset=i)};

      f(~iter, node);
    },
    nodes,
  );
};

let map = (f, t) => {
  let nodes = Array.of_list(t);
  let replace = (~offset, n, value) => nodes[offset + n + 1] = value;
  let peek = (~offset, n) => nodes[offset + n + 1];

  let res =
    Array.mapi(
      (i, node) => {
        let iter = {replace: replace(~offset=i), peek: peek(~offset=i)};

        f(~iter, node);
      },
      nodes,
    );

  Array.to_list(res);
};
