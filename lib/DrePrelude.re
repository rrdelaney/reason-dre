let types = {
  open DynamicScope;

  let ts = ref([]);
  let push = t => ts := [t, ...ts^];

  /* These types are all in Reason, but not JS. */
  push({dreName: "Js.Promise", reasonName: "Js.Promise.t"});
  push({dreName: "Js.Dict", reasonName: "Js.Dict.t"});
  push({dreName: "int", reasonName: "int"});
  push({dreName: "float", reasonName: "float"});
  push({dreName: "unit", reasonName: "unit"});
  push({dreName: "bool", reasonName: "bool"});
  push({dreName: "Dom.element", reasonName: "Dom.element"});

  /* Some compatability types that are parsed for newcomers. */
  if (! DreConfig.strictTypes) {
    push({dreName: "Promise", reasonName: "Js.Promise.t"});
  };

  ts^;
};
