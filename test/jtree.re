/* This code is in the public domain */

/* Generic JSON tree type */

type json = [
  | `Null
  | `Bool(bool)
  | `Float(float)
  | `String(string)
  | `A(list(json))
  | `O(list((string, json)))
];

exception Escape(((int, int), (int, int)), Jsonm.error);

let json_of_src =
    (~encoding=?, src: [ | `Channel(in_channel) | `String(string)]) => {
  let dec = d =>
    switch (Jsonm.decode(d)) {
    | `Lexeme(l) => l
    | `Error(e) =>
      raise( Escape(Jsonm.decoded_range(d), e))
    | `End
    | `Await => assert(false)
    };

  let rec value = (v, k, d) =>
    switch (v) {
    | `Os => obj([], k, d)
    | `As => arr([], k, d)
    | (`Null | `Bool(_) | `String(_) | `Float(_)) as v => k(v, d)
    | _ => assert(false)
    }
  and arr = (vs, k, d) =>
    switch (dec(d)) {
    | `Ae => k(`A(List.rev(vs)), d)
    | v => value(v, v => arr([v, ...vs], k), d)
    }
  and obj = (ms, k, d) =>
    switch (dec(d)) {
    | `Oe => k(`O(List.rev(ms)), d)
    | `Name(n) => value(dec(d), v => obj([(n, v), ...ms], k), d)
    | _ => assert(false)
    };

  let d = Jsonm.decoder(~encoding?, src);
  try(`JSON(value(dec(d), (v, _) => v, d))) {
  |  Escape(r, e) => `Error((r, e))
  };
};

let json_to_dst =
    (
      ~minify,
      dst: [ | `Channel(out_channel) | `Buffer(Buffer.t)],
      json: json,
    ) => {
  let enc = (e, l) => ignore(Jsonm.encode(e, `Lexeme(l)));
  let rec value = (v, k, e) =>
    switch (v) {
    | `A(vs) => arr(vs, k, e)
    | `O(ms) => obj(ms, k, e)
    | (`Null | `Bool(_) | `Float(_) | `String(_)) as v =>
      enc(e, v);
      k(e);
    }
  and arr = (vs, k, e) => {
    enc(e, `As);
    arr_vs(vs, k, e);
  }
  and arr_vs = (vs, k, e) =>
    switch (vs) {
    | [v, ...vs'] => value(v, arr_vs(vs', k), e)
    | [] =>
      enc(e, `Ae);
      k(e);
    }
  and obj = (ms, k, e) => {
    enc(e, `Os);
    obj_ms(ms, k, e);
  }
  and obj_ms = (ms, k, e) =>
    switch (ms) {
    | [(n, v), ...ms] =>
      enc(e, `Name(n));
      value(v, obj_ms(ms, k), e);
    | [] =>
      enc(e, `Oe);
      k(e);
    };

  let e = Jsonm.encoder(~minify, dst);
  let finish = e => ignore(Jsonm.encode(e, `End));
  value(json, finish, e);
};

let main = () => {
  let exec = Filename.basename(Sys.executable_name);
  let usage =
    Printf.sprintf(
      "Usage: %s [OPTION]...\n Recode JSON from stdin to stdout via an in-memory tree representation.\nOptions:",
      exec,
    );

  let minify = ref(true);
  let options = [("-pp", Arg.Clear(minify), " Pretty print output")];
  let anon = _ => raise(Arg.Bad("illegal argument"));
  Arg.parse(Arg.align(options), anon, usage);
  let minify = minify^;
  switch (json_of_src(`Channel(stdin))) {
  | `JSON(j) => json_to_dst(~minify, `Channel(stdout), j)
  | `Error(((l1, c1), (l2, c2)), e) =>
    Format.eprintf("-:%d.%d-%d.%d: %a\n%!", l1, c1, l2, c2, Jsonm.pp_error, e)
  };
};

let () = main();
