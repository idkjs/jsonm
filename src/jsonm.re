/*---------------------------------------------------------------------------
   Copyright (c) 2012 The jsonm programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/

/* Braced non-terminals in comments refer to RFC 4627 non-terminals. */

let io_buffer_size = 65536; /* IO_BUFFER_SIZE 4.0.0 */
let pp = Format.fprintf;

/* Unsafe string and bytes manipulations. If you don't believe the authors's
   invariants, replacing with safe versions makes everything safe in the
   module. He won't be upset. */

let unsafe_byte = (s, j) => Char.code(String.unsafe_get(s, j));

let unsafe_blit = (s, soff, d, doff) =>
  Bytes.unsafe_blit(Bytes.unsafe_of_string(s), soff, d, doff);

let unsafe_set_byte = (s, j, byte) =>
  Bytes.unsafe_set(s, j, Char.unsafe_chr(byte));

/* Characters and their classes */

let ux_eoi = max_int; /* End of input, outside unicode range. */
let ux_soi = max_int - 1; /* Start of input, outside unicode range. */
let u_nl = 0x0A; /* \n */
let u_sp = 0x20; /*   */
let u_quot = 0x22; /* '' */
let u_lbrack = 0x5B; /* [ */
let u_rbrack = 0x5D; /* ] */
let u_lbrace = 0x7B; /* { */
let u_rbrace = 0x7D; /* } */
let u_colon = 0x3A; /* : */
let u_dot = 0x2E; /* . */
let u_comma = 0x2C; /* , */
let u_minus = 0x2D; /* - */
let u_slash = 0x2F; /* / */
let u_bslash = 0x5C; /* \ */
let u_times = 0x2A; /* * */
let u_rep = Uchar.to_int(Uutf.u_rep);

let must_escape = u => u <= 0x1F || u == 0x22 || u == 0x5C;
let is_digit = u => 0x30 <= u && u <= 0x39;
let is_hex_digit = u =>
  0x30 <= u && u <= 0x39 || 0x41 <= u && u <= 0x46 || 0x61 <= u && u <= 0x66;

let is_white =
  fun /* N.B. Uutf normalizes U+000D to U+000A. */
  | 0x20
  | 0x09
  | 0x0A => true
  | _ => false;

let is_val_sep =
  fun /* N.B. Uutf normalizes U+000D to U+000A. */
  | 0x20
  | 0x09
  | 0x0A
  | 0x2C
  | 0x5D
  | 0x7D => true
  | _ => false;

/* Data model */

type lexeme = [
  | `Null
  | `Bool(bool)
  | `String(string)
  | `Float(float)
  | `Name(string)
  | `As
  | `Ae
  | `Os
  | `Oe
];

let pp_lexeme = ppf =>
  fun
  | `Null => pp(ppf, "`Null")
  | `Bool(b) => pp(ppf, "@[`Bool %b@]", b)
  | `String(s) => pp(ppf, "@[`String %S@]", s)
  | `Name(s) => pp(ppf, "@[`Name %S@]", s)
  | `Float(f) => pp(ppf, "@[`Float %s@]", Js.Float.toString(f))
  | `As => pp(ppf, "`As")
  | `Ae => pp(ppf, "`Ae")
  | `Os => pp(ppf, "`Os")
  | `Oe => pp(ppf, "`Oe");

/* Decode */

type error = [
  | `Illegal_BOM
  | `Illegal_escape(
      [
        | `Not_hex_uchar(Uchar.t)
        | `Not_esc_uchar(Uchar.t)
        | `Not_lo_surrogate(int)
        | `Lone_lo_surrogate(int)
        | `Lone_hi_surrogate(int)
      ],
    )
  | `Illegal_string_uchar(Uchar.t)
  | `Illegal_bytes(string)
  | `Illegal_literal(string)
  | `Illegal_number(string)
  | `Unclosed([ | `As | `Os | `String | `Comment])
  | `Expected(
      [
        | `Comment
        | `Value
        | `Name
        | `Name_sep
        | `Json
        | `Eoi
        | `Aval(bool)
        | `Omem(bool /* [true] if first object member */)
      ],
    ) /* [true] if first array value  */
];

let err_bom = `Error(`Illegal_BOM);
let err_not_hex = u =>
  `Error(`Illegal_escape(`Not_hex_uchar(Uchar.of_int(u))));
let err_not_esc = u =>
  `Error(`Illegal_escape(`Not_esc_uchar(Uchar.of_int(u))));
let err_not_lo = p => `Error(`Illegal_escape(`Not_lo_surrogate(p)));
let err_lone_lo = p => `Error(`Illegal_escape(`Lone_lo_surrogate(p)));
let err_lone_hi = p => `Error(`Illegal_escape(`Lone_hi_surrogate(p)));
let err_str_char = u => `Error(`Illegal_string_uchar(Uchar.of_int(u)));
let err_bytes = bs => `Error(`Illegal_bytes(bs));
let err_unclosed_comment = `Error(`Unclosed(`Comment));
let err_unclosed_string = `Error(`Unclosed(`String));
let err_unclosed_arr = `Error(`Unclosed(`As));
let err_unclosed_obj = `Error(`Unclosed(`Os));
let err_number = s => `Error(`Illegal_number(s));
let err_literal = s => `Error(`Illegal_literal(s));
let err_exp_comment = `Error(`Expected(`Comment));
let err_exp_value = `Error(`Expected(`Value));
let err_exp_name = `Error(`Expected(`Name));
let err_exp_nsep = `Error(`Expected(`Name_sep));
let err_exp_arr_fst = `Error(`Expected(`Aval(true)));
let err_exp_arr_nxt = `Error(`Expected(`Aval(false)));
let err_exp_obj_fst = `Error(`Expected(`Omem(true)));
let err_exp_obj_nxt = `Error(`Expected(`Omem(false)));
let err_exp_json = `Error(`Expected(`Json));
let err_exp_eoi = `Error(`Expected(`Eoi));

let pp_cp = (ppf, u) => pp(ppf, "U+%04X", u);
let pp_uchar = (ppf, u) =>
  if (Uchar.to_int(u) <= 0x1F) /* most control chars */ {
    pp_cp(ppf, Uchar.to_int(u));
  } else {
    let b = Buffer.create(4);
    Uutf.Buffer.add_utf_8(b, u);
    pp(ppf, "'%s' (%a)", Buffer.contents(b), pp_cp, Uchar.to_int(u));
  };

let pp_error = ppf =>
  fun
  | `Illegal_BOM =>
    pp(ppf, "@[illegal@ initial@ BOM@ in@ character@ stream@]")
  | `Illegal_escape(r) => {
      pp(ppf, "@[illegal@ escape,@ ");
      switch (r) {
      | `Not_hex_uchar(u) => pp(ppf, "%a@ not@ a@ hex@ digit@]", pp_uchar, u)
      | `Not_esc_uchar(u) =>
        pp(ppf, "%a@ not@ an@ escaped@ character@]", pp_uchar, u)
      | `Lone_lo_surrogate(p) =>
        pp(ppf, "%a@ lone@ low@ surrogate@]", pp_cp, p)
      | `Lone_hi_surrogate(p) =>
        pp(ppf, "%a@ lone@ high@ surrogate@]", pp_cp, p)
      | `Not_lo_surrogate(p) =>
        pp(ppf, "%a@ not@ a@ low@ surrogate@]", pp_cp, p)
      };
    }
  | `Illegal_string_uchar(u) =>
    pp(ppf, "@[illegal@ character@ in@ JSON@ string@ (%a)@]", pp_uchar, u)
  | `Illegal_bytes(bs) => {
      let l = String.length(bs);
      pp(ppf, "@[illegal@ bytes@ in@ character@ stream@ (");
      if (l > 0) {
        pp(ppf, "%02X", Char.code(bs.[0]));
      };
      for (i in 1 to l - 1) {
        pp(ppf, " %02X", Char.code(bs.[i]));
      };
      pp(ppf, ")@]");
    }
  | `Illegal_number(n) => pp(ppf, "@[illegal@ number@ (%s)@]", n)
  | `Illegal_literal(l) => pp(ppf, "@[illegal@ literal@ (%s)@]", l)
  | `Unclosed(r) => {
      pp(ppf, "@[unclosed@ ");
      switch (r) {
      | `As => pp(ppf, "array@]")
      | `Os => pp(ppf, "object@]")
      | `String => pp(ppf, "string@]")
      | `Comment => pp(ppf, "comment@]")
      };
    }
  | `Expected(r) => {
      pp(ppf, "@[expected@ ");
      switch (r) {
      | `Comment => pp(ppf, "JavaScript@ comment@]")
      | `Value => pp(ppf, "JSON@ value@]")
      | `Name => pp(ppf, "member@ name@]")
      | `Name_sep => pp(ppf, "name@ separator@ (':')@]")
      | `Aval(true) => pp(ppf, "value@ or@ array@ end@ (value@ or@ ']')@]")
      | `Aval(false) =>
        pp(ppf, "value@ separator@ or@ array@ end@ (','@ or@ ']')@]")
      | `Omem(true) =>
        pp(ppf, "member@ name@ or@ object@ end@ ('\"'@ or@ '}')@]")
      | `Omem(false) =>
        pp(ppf, "value@ separator@ or@ object@ end@ (','@ or@ '}')@]")
      | `Json => pp(ppf, "JSON@ text (JSON value)@]")
      | `Eoi => pp(ppf, "end@ of@ input@]")
      };
    };

type pos = (int, int);
type encoding = [ | `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE];
type src = [ | `Channel(in_channel) | `String(string) | `Manual];
type decode = [ | `Await | `End | `Lexeme(lexeme) | `Error(error)];
type uncut = [ | `Comment([ | `M | `S], string) | `White(string)];

let pp_decode = ppf =>
  fun
  | `Lexeme(l) => pp(ppf, "@[`Lexeme @[(%a)@]@]", pp_lexeme, l)
  | `Await => pp(ppf, "`Await")
  | `End => pp(ppf, "`End")
  | `Error(e) => pp(ppf, "@[`Error @[(%a)@]@]", pp_error, e)
  | `White(s) => pp(ppf, "@[`White @[%S@]@]", s)
  | `Comment(style, s) => {
      let pr_style = ppf => (
        fun
        | `M => pp(ppf, "`M")
        | `S => pp(ppf, "`S")
      );
      pp(ppf, "@[`Comment @[(%a, %S)@]@]", pr_style, style, s);
    };

type decoder = {
  u: Uutf.decoder, /* Unicode character decoder. */
  buf: Buffer.t, /* string accumulation buffer. */
  mutable uncut: bool, /* [true] to bufferize comments and white space. */
  mutable s_line: int, /* last saved start line. */
  mutable s_col: int, /* last saved start column. */
  mutable e_line: int, /* last saved end line. */
  mutable e_col: int, /* last saved end column. */
  mutable c: int, /* character lookahead. */
  mutable stack:
    /* stack of open arrays and objects. */
    list([ | `As(pos) | `Os(pos)]),
  mutable next_name: bool, /* [true] if next decode should be [`Name]. */
  mutable last_start: bool, /* [true] if last lexeme was `As or `Os. */
  mutable k:
    /* decoder continuation. */
    decoder => [ decode | uncut],
};

let baddc = (d, c) => Uutf.Buffer.add_utf_8(d.buf, Uchar.unsafe_of_int(c));
let badd = d => Uutf.Buffer.add_utf_8(d.buf, Uchar.unsafe_of_int(d.c));
let buf = d => {
  let t = Buffer.contents(d.buf);
  Buffer.clear(d.buf);
  t;
};
let dpos = d => (Uutf.decoder_line(d.u), Uutf.decoder_col(d.u));
let spos = d => {
  d.s_line = Uutf.decoder_line(d.u);
  d.s_col = Uutf.decoder_col(d.u);
};
let epos = d => {
  d.e_line = Uutf.decoder_line(d.u);
  d.e_col = Uutf.decoder_col(d.u);
};
let stack_range = d =>
  switch (d.stack) {
  | [] => assert(false)
  | [`As(l, c), ..._]
  | [`Os(l, c), ..._] =>
    d.s_line = l;
    d.s_col = c;
    epos(d);
  };

let dpop = d =>
  switch (
    {
      spos(d);
      epos(d);
      d.stack;
    }
  ) {
  | [_, ...[`Os(_), ..._] as ss] =>
    d.next_name = true;
    d.stack = ss;
  | [_, ...[`As(_), ..._] as ss] =>
    d.next_name = false;
    d.stack = ss;
  | [_] =>
    d.next_name = false;
    d.stack = [];
  | [] => assert(false)
  };

let ret_eoi = d => `End;
let ret = (v: [< decode | uncut], k, d) => {
  d.k = k;
  v;
};
let rec readc = (k, d) =>
  switch (Uutf.decode(d.u)) {
  | `Uchar(u) =>
    d.c = Uchar.to_int(u);
    k(d);
  | `End =>
    d.c = ux_eoi;
    k(d);
  | `Await => ret(`Await, readc(k), d)
  | `Malformed(bs) =>
    d.c = u_rep;
    epos(d);
    ret(err_bytes(bs), k, d);
  };

let rec r_scomment = (k, d) =>
  /* single line comment. // was eaten. */
  if (d.c != u_nl && d.c != ux_eoi) {
    badd(d);
    readc(r_scomment(k), d);
  } else {
    epos(d);
    ret(`Comment((`S, buf(d))), readc(k), d);
  };

let rec r_mcomment = (closing, k, d) =>

  if (d.c == ux_eoi) {
    epos(d);
    ret(err_unclosed_comment, ret_eoi, d);
  } else if (closing) {
    if (d.c == u_slash) {
      epos(d);
      ret(`Comment((`M, buf(d))), readc(k), d);
    } else if (d.c == u_times) {
      badd(d);
      readc(r_mcomment(true, k), d);
    } else {
      baddc(d, u_times);
      badd(d);
      readc(r_mcomment(false, k), d);
    };
  } else if (d.c == u_times) {
    readc(r_mcomment(true, k), d);
  } else {
    badd(d);
    readc(r_mcomment(false, k), d);
  };

let r_comment = (k, d) =>
  /* comment, / was eaten. */
  if (d.c == u_slash) {
    readc(r_scomment(k), d);
  } else if (d.c == u_times) {
    readc(r_mcomment(false, k), d);
  } else {
    epos(d);
    ret(err_exp_comment, k, d);
  };

let rec r_ws_uncut = (k, d) =>
  if (is_white(d.c)) {
    epos(d);
    badd(d);
    readc(r_ws_uncut(k), d);
  } else {
    ret(`White(buf(d)), k, d);
  };

let rec r_white_uncut = (k, d) =>
  /* {ws} / comment */
  if (is_white(d.c)) {
    spos(d);
    r_ws_uncut(r_white_uncut(k), d);
  } else if (d.c == u_slash) {
    spos(d);
    readc(r_comment(r_white_uncut(k)), d);
  } else {
    k(d);
  };

let rec r_ws = (k, d) =>
  if (is_white(d.c)) {
    readc(r_ws(k), d);
  } else {
    k(d);
  }; /* {ws} */
let r_white = (k, d) =>
  if (d.uncut) {
    r_white_uncut(k, d);
  } else {
    r_ws(k, d);
  };

let rec r_u_escape = (hi, u, count, k, d) => {
  /* unicode escapes. */
  let error = (err, k, d) => {
    baddc(d, u_rep);
    ret(err, k, d);
  };
  if (count > 0) {
    if (!is_hex_digit(d.c)) {
      epos(d);
      error(err_not_hex(d.c), readc(k), d);
    } else {
      let u =
        u
        * 16
        + (
          if (d.c <= 0x39) /* 9 */ {
            d.c - 0x30;
          } else if (d.c <= 0x46 /* F */) {
            d.c - 0x37;
          } else {
            d.c - 0x57;
          }
        );

      epos(d);
      readc(r_u_escape(hi, u, count - 1, k), d);
    };
  } else {
    switch (hi) {
    | Some(hi) =>
      /* combine high and low surrogate into scalar value. */
      if (u < 0xDC00 || u > 0xDFFF) {
        error(err_not_lo(u), k, d);
      } else {
        let u = (hi land 0x3FF) lsl 10 lor (u land 0x3FF) + 0x10000;
        baddc(d, u);
        k(d);
      }
    | None =>
      if (u < 0xD800 || u > 0xDFFF) {
        baddc(d, u);
        k(d);
      } else if (u > 0xDBFF) {
        error(err_lone_lo(u), k, d);
      } else if (d.c != u_bslash) {
        error(err_lone_hi(u), k, d);
      } else {
        readc(
          d =>
            if (d.c != 0x75) /* u */ {
              error(err_lone_hi(u), r_escape(k), d);
            } else {
              readc(r_u_escape(Some(u), 0, 4, k), d);
            },
          d,
        );
      }
    };
  };
}

and r_escape = (k, d) =>
  switch (d.c) {
  | 0x22 /* '' */ =>
    baddc(d, u_quot);
    readc(k, d);
  | 0x5C /* \ */ =>
    baddc(d, u_bslash);
    readc(k, d);
  | 0x2F /* / */ =>
    baddc(d, u_slash);
    readc(k, d);
  | 0x62 /* b */ =>
    baddc(d, 0x08);
    readc(k, d);
  | 0x66 /* f */ =>
    baddc(d, 0x0C);
    readc(k, d);
  | 0x6E /* n */ =>
    baddc(d, u_nl);
    readc(k, d);
  | 0x72 /* r */ =>
    baddc(d, 0x0D);
    readc(k, d);
  | 0x74 /* t */ =>
    baddc(d, 0x09);
    readc(k, d);
  | 0x75 /* u */ => readc(r_u_escape(None, 0, 4, k), d)
  | c =>
    epos(d);
    baddc(d, u_rep);
    ret(err_not_esc(c), readc(k), d);
  };

let rec r_string = (k, d) =>
  /* {string}, '' eaten. */
  if (d.c == ux_eoi) {
    epos(d);
    ret(err_unclosed_string, ret_eoi, d);
  } else if (!must_escape(d.c)) {
    badd(d);
    readc(r_string(k), d);
  } else if (d.c == u_quot) {
    epos(d);
    readc(k, d);
  } else if (d.c == u_bslash) {
    readc(r_escape(r_string(k)), d);
  } else {
    epos(d);
    baddc(d, u_rep);
    ret(err_str_char(d.c), readc(r_string(k)), d);
  };

let rec r_float = (k, d) =>
  /* {number} */
  if (!is_val_sep(d.c) && d.c != ux_eoi) {
    epos(d);
    badd(d);
    readc(r_float(k), d);
  } else {
    let s = buf(d);
    try(ret(`Lexeme(`Float(float_of_string(s))), k, d)) {
    | Failure(_) => ret(err_number(s), k, d)
    };
  };

let rec r_literal = (k, d) =>
  /* {true} / {false} / {null} */
  if (!is_val_sep(d.c) && d.c != ux_eoi) {
    epos(d);
    badd(d);
    readc(r_literal(k), d);
  } else {
    switch (buf(d)) {
    | "true" => ret(`Lexeme(`Bool(true)), k, d)
    | "false" => ret(`Lexeme(`Bool(false)), k, d)
    | "null" => ret(`Lexeme(`Null), k, d)
    | s => ret(err_literal(s), k, d)
    };
  };

let rec r_value = (err, k, d) =>
  switch (d.c) {
  /* {value} */
  | 0x5B /* [ */ =>
    /* {begin-array} */
    spos(d);
    epos(d);
    d.last_start = true;
    d.stack = [`As(dpos(d)), ...d.stack];
    ret(`Lexeme(`As), readc(k), d);
  | 0x7B /* { */ =>
    /* {begin-object} */
    spos(d);
    epos(d);
    d.last_start = true;
    d.next_name = true;
    d.stack = [`Os(dpos(d)), ...d.stack];
    ret(`Lexeme(`Os), readc(k), d);
  | 0x22 /* '' */ =>
    let lstring = (k, d) => ret(`Lexeme(`String(buf(d))), k, d);
    spos(d);
    readc(r_string(lstring(k)), d);
  | 0x66 /* f */
  | 0x6E /* n */
  | 0x74 /* t */ =>
    spos(d);
    r_literal(k, d);
  | u when is_digit(u) || u == u_minus =>
    spos(d);
    r_float(k, d);
  | u => err(k, d)
  };

let rec discard_to = (c1, c2, err, k, d) =>
  if (d.c == c1 || d.c == c2 || d.c == ux_eoi) {
    ret(err, k, d);
  } else {
    epos(d);
    readc(discard_to(c1, c2, err, k), d);
  };

let r_arr_val = (k, d) => {
  /* [{value-separator}] {value} / {end-array} */
  let nxval = (err, k, d) => {
    spos(d);
    discard_to(u_comma, u_rbrack, err, k, d);
  };
  let last_start = d.last_start;
  d.last_start = false;
  if (d.c == ux_eoi) {
    stack_range(d);
    ret(err_unclosed_arr, ret_eoi, d);
  } else if (d.c == u_rbrack) {
    dpop(d);
    ret(`Lexeme(`Ae), readc(k), d);
  } else if (last_start) {
    r_value(nxval(err_exp_arr_fst), k, d);
  } else if (d.c == u_comma) {
    readc(r_white(r_value(nxval(err_exp_value), k)), d);
  } else {
    nxval(err_exp_arr_nxt, k, d);
  };
};

let nxmem = (err, k, d) => {
  spos(d);
  d.next_name = true;
  discard_to(u_comma, u_rbrace, err, k, d);
};

let r_obj_value = (k, d) => {
  /* {name-separator} {value} */
  d.next_name = true;
  if (d.c == u_colon) {
    readc(r_white(r_value(nxmem(err_exp_value), k)), d);
  } else {
    nxmem(err_exp_nsep, k, d);
  };
};

let r_obj_name = (k, d) => {
  /* [{value-separator}] string / end-object */
  let r_name = (err, k, d) => {
    let ln = (k, d) => ret(`Lexeme(`Name(buf(d))), k, d);
    if (d.c != u_quot) {
      nxmem(err, k, d);
    } else {
      spos(d);
      readc(r_string(ln(k)), d);
    };
  };

  let last_start = d.last_start;
  d.last_start = false;
  d.next_name = false;
  if (d.c == ux_eoi) {
    stack_range(d);
    ret(err_unclosed_obj, ret_eoi, d);
  } else if (d.c == u_rbrace) {
    dpop(d);
    ret(`Lexeme(`Oe), readc(k), d);
  } else if (last_start) {
    r_name(err_exp_obj_fst, k, d);
  } else if (d.c == u_comma) {
    readc(r_white(r_name(err_exp_name, k)), d);
  } else {
    nxmem(err_exp_obj_nxt, k, d);
  };
};

let r_end = (k, d) =>
  /* end of input */
  if (d.c == ux_eoi) {
    ret(`End, ret_eoi, d);
  } else {
    let drain = (k, d) => {
      spos(d);
      discard_to(ux_eoi, ux_eoi, err_exp_eoi, k, d);
    };
    drain(ret_eoi, d);
  };

let rec r_lexeme = d =>
  switch (d.stack) {
  | [`As(_), ..._] => r_white(r_arr_val(r_lexeme), d)
  | [`Os(_), ..._] =>
    if (d.next_name) {
      r_white(r_obj_name(r_lexeme), d);
    } else {
      r_white(r_obj_value(r_lexeme), d);
    }
  | [] => r_white(r_end(r_lexeme), d)
  };

let rec discard_to_white = (err, k, d) =>
  if (is_white(d.c) || d.c == ux_eoi) {
    ret(err, k, d);
  } else {
    epos(d);
    readc(discard_to_white(err, k), d);
  };

let rec r_json = (k, d) => {
  /* {value} */
  let err = (k, d) => {
    spos(d);
    discard_to_white(err_exp_json, r_white(r_json(k)), d);
  };
  if (d.c != ux_eoi) {
    r_value(err, k, d);
  } else {
    ret(err_exp_json, k, d);
  };
};

let r_start = d => {
  /* start of input */
  let bom = (k, d) =>
    if (Uutf.decoder_removed_bom(d.u)) {
      ret(err_bom, k, d);
    } else {
      k(d);
    };
  readc(bom(r_white(r_json(r_lexeme))), d);
};

let nln = `ASCII(Uchar.unsafe_of_int(0x000A));
let decoder = (~encoding=?, src) => {
  let u = Uutf.decoder(~encoding?, ~nln, src);
  {
    u,
    buf: Buffer.create(1024),
    uncut: false,
    s_line: 1,
    s_col: 0,
    e_line: 1,
    e_col: 0,
    c: ux_soi,
    next_name: false,
    last_start: false,
    stack: [],
    k: r_start,
  };
};

let decode_uncut = d => {
  d.uncut = true;
  d.k(d);
};
let rec decode = d =>
  switch (
    {
      d.uncut = false;
      d.k(d);
    }
  ) {
  | #decode as v => (v :> [> decode])
  | `Comment(_)
  | `White(_) => assert(false)
  };

let decoder_src = d => Uutf.decoder_src(d.u);
let decoded_range = d => ((d.s_line, d.s_col), (d.e_line, d.e_col));
let decoder_encoding = d =>
  switch (Uutf.decoder_encoding(d.u)) {
  | #encoding as enc => enc
  | `US_ASCII
  | `ISO_8859_1 => assert(false)
  };

/* Encode */

let invalid_arg = fmt => {
  let b = Buffer.create(20); /* for thread safety. */
  let ppf = Format.formatter_of_buffer(b);
  let k = ppf => {
    Format.pp_print_flush(ppf, ());
    invalid_arg(Buffer.contents(b));
  };
  Format.kfprintf(k, ppf, fmt);
};

let invalid_bounds = (j, l) =>
  invalid_arg("invalid bounds (index %d, length %d)", j, l);
let expect = (e, v) =>
  invalid_arg("%a encoded but expected %s", pp_decode, v, e);
let expect_await = v => expect("`Await", v);
let expect_end = l => expect("`End", `Lexeme(l));
let expect_mem_value = l =>
  expect("any `Lexeme but `Name, `Oe or `Ae", `Lexeme(l));
let expect_arr_value_ae = l =>
  expect("any `Lexeme but `Name or `Oe", `Lexeme(l));
let expect_name_or_oe = l => expect("`Lexeme (`Name _ | `Oe)", `Lexeme(l));
let expect_json = v =>
  expect("`Lexeme (`Null | `Bool _ | `Float _ | `String _ | `As | `Os)", v);

let expect_lend = (lstart, v) =>
  expect(
    if (lstart == `As) {
      "`Lexeme `Ae";
    } else {
      "`Lexeme `Oe";
    },
    v,
  );

type dst = [ | `Channel(out_channel) | `Buffer(Buffer.t) | `Manual];
type encode = [ | `Await | `End | `Lexeme(lexeme)];
type encoder = {
  dst, /* output destination. */
  minify: bool, /* [true] for compact output. */
  mutable o: Bytes.t, /* current output chunk. */
  mutable o_pos: int, /* next output position to write. */
  mutable o_max: int, /* maximal output position to write. */
  buf: Buffer.t, /* buffer to format floats. */
  mutable stack: list([ | `As | `Os]), /* stack of open arrays and objects. */
  mutable nest: int, /* nesting level (String.length stack). */
  mutable next_name: bool, /* [true] if next encode should `Name. */
  mutable last_start: bool, /* [true] if last encode was [`As | `Os]. */
  mutable k:
    /* decoder continuation. */
    (encoder, [ encode | uncut]) => [ | `Ok | `Partial],
};

let o_rem = e => e.o_max - e.o_pos + 1; /* remaining bytes to write in [e.o]. */
let dst = (e, s, j, l) => {
  /* set [e.o] with [s]. */
  if (j < 0 || l < 0 || j + l > Bytes.length(s)) {
    invalid_bounds(j, l);
  };
  e.o = s;
  e.o_pos = j;
  e.o_max = j + l - 1;
};

let partial = (k, e) =>
  fun
  | `Await => k(e)
  | v => expect_await(v);
let flush = (k, e) =>
  switch (e.dst) {
  /* get free space in [d.o] and [k]ontinue. */
  | `Manual =>
    e.k = partial(k);
    `Partial;
  | `Channel(oc) =>
    output(oc, e.o, 0, e.o_pos);
    e.o_pos = 0;
    k(e);
  | `Buffer(b) =>
    let o = Bytes.unsafe_to_string(e.o);
    Buffer.add_substring(b, o, 0, e.o_pos);
    e.o_pos = 0;
    k(e);
  };

let rec writeb = (b, k, e) =>
  /* write byte [b] and [k]ontinue. */
  if (e.o_pos > e.o_max) {
    flush(writeb(b, k), e);
  } else {
    unsafe_set_byte(e.o, e.o_pos, b);
    e.o_pos = e.o_pos + 1;
    k(e);
  };

let rec writes = (s, j, l, k, e) => {
  /* write [l] bytes from [s] starting at [j]. */
  let rem = o_rem(e);
  if (rem >= l) {
    unsafe_blit(s, j, e.o, e.o_pos, l);
    e.o_pos = e.o_pos + l;
    k(e);
  } else {
    unsafe_blit(s, j, e.o, e.o_pos, rem);
    e.o_pos = e.o_pos + rem;
    flush(writes(s, j + rem, l - rem, k), e);
  };
};

let rec writebuf = (j, l, k, e) => {
  /* write [l] bytes from [e.buf] starting at [j]. */
  let rem = o_rem(e);
  if (rem >= l) {
    Buffer.blit(e.buf, j, e.o, e.o_pos, l);
    e.o_pos = e.o_pos + l;
    k(e);
  } else {
    Buffer.blit(e.buf, j, e.o, e.o_pos, rem);
    e.o_pos = e.o_pos + rem;
    flush(writebuf(j + rem, l - rem, k), e);
  };
};

let w_indent = (k, e) => {
  let rec loop = (indent, k, e) => {
    let spaces = (e, indent) => {
      let max = e.o_pos + indent - 1;
      for (j in e.o_pos to max) {
        unsafe_set_byte(e.o, j, u_sp);
      };
      e.o_pos = max + 1;
    };

    let rem = o_rem(e);
    if (rem < indent) {
      spaces(e, rem);
      flush(loop(indent - rem, k), e);
    } else {
      spaces(e, indent);
      k(e);
    };
  };

  loop(e.nest * 2, k, e);
};

let rec w_json_string = (s, k, e) => {
  /* escapes as mandated by the standard. */
  let rec loop = (s, j, pos, max, k, e) =>
    if (pos > max) {
      if (j > max) {
        k(e);
      } else {
        writes(s, j, pos - j, k, e);
      };
    } else {
      let next = pos + 1;
      let escape = esc =>
        /* assert (String.length esc = 2 ). */
        writes(
          s,
          j,
          pos - j,
          writes(esc, 0, 2, loop(s, next, next, max, k)),
          e,
        );

      switch (unsafe_byte(s, pos)) {
      | 0x22 => escape("\\\"")
      | 0x5C => escape("\\\\")
      | 0x0A => escape("\\n")
      | c when c <= 0x1F =>
        let hex = d =>
          if (d < 10) {
            0x30 + d;
          } else {
            0x41 + (d - 10);
          };
        writes(
          s,
          j,
          pos - j,
          writes(
            "\\u00",
            0,
            4,
            writeb(
              hex(c lsr 4),
              writeb(hex(c land 0xF), loop(s, next, next, max, k)),
            ),
          ),
          e,
        );
      | c => loop(s, j, next, max, k, e)
      };
    };

  writeb(
    u_quot,
    loop(s, 0, 0, String.length(s) - 1, writeb(u_quot, k)),
    e,
  );
};

let w_name = (n, k, e) => {
  e.last_start = false;
  e.next_name = false;
  w_json_string(n, writeb(u_colon, k), e);
};

let w_value = (~in_obj, l, k, e) =>
  switch (l) {
  | `String(s) =>
    e.last_start = false;
    e.next_name = in_obj;
    w_json_string(s, k, e);
  | `Bool(b) =>
    e.last_start = false;
    e.next_name = in_obj;
    if (b) {
      writes("true", 0, 4, k, e);
    } else {
      writes("false", 0, 5, k, e);
    };
  | `Float(f) =>
    e.last_start = false;
    e.next_name = in_obj;
    Buffer.clear(e.buf);
    Printf.bprintf(e.buf, "%.16g", f);
    writebuf(0, Buffer.length(e.buf), k, e);
  | `Os =>
    e.last_start = true;
    e.next_name = true;
    e.nest = e.nest + 1;
    e.stack = [`Os, ...e.stack];
    writeb(u_lbrace, k, e);
  | `As =>
    e.last_start = true;
    e.next_name = false;
    e.nest = e.nest + 1;
    e.stack = [`As, ...e.stack];
    writeb(u_lbrack, k, e);
  | `Null =>
    e.last_start = false;
    e.next_name = in_obj;
    writes("null", 0, 4, k, e);
  | (`Oe | `Ae | `Name(_)) as l =>
    if (in_obj) {
      expect_mem_value(l);
    } else {
      expect_arr_value_ae(l);
    }
  };

let w_lexeme = (k, e, l) => {
  let epop = e => {
    e.last_start = false;
    e.nest = e.nest - 1;
    e.stack = List.tl(e.stack);
    switch (e.stack) {
    | [`Os, ..._] => e.next_name = true
    | _ => e.next_name = false
    };
  };

  switch (List.hd(e.stack)) {
  | `Os =>
    /* inside object. */
    if (!e.next_name) {
      w_value(~in_obj=true, l, k, e);
    } else {
      switch (l) {
      | `Name(n) =>
        let name = (n, k, e) =>
          if (e.minify) {
            w_name(n, k, e);
          } else {
            writeb(u_nl, w_indent(w_name(n, writeb(u_sp, k))), e);
          };

        if (e.last_start) {
          name(n, k, e);
        } else {
          writeb(u_comma, name(n, k), e);
        };
      | `Oe =>
        if (e.minify || e.last_start) {
          epop(e);
          writeb(u_rbrace, k, e);
        } else {
          epop(e);
          writeb(u_nl, w_indent(writeb(u_rbrace, k)), e);
        }
      | v => expect_name_or_oe(l)
      };
    }
  | `As =>
    /* inside array. */
    switch (l) {
    | `Ae =>
      if (e.minify || e.last_start) {
        epop(e);
        writeb(u_rbrack, k, e);
      } else {
        epop(e);
        writeb(u_nl, w_indent(writeb(u_rbrack, k)), e);
      }
    | l =>
      let value = (l, k, e) =>
        if (e.minify) {
          w_value(~in_obj=false, l, k, e);
        } else {
          writeb(u_nl, w_indent(w_value(~in_obj=false, l, k)), e);
        };

      if (e.last_start) {
        value(l, k, e);
      } else {
        writeb(u_comma, value(l, k), e);
      };
    }
  };
};

let rec encode_ = (k, e) =>
  fun
  | `Lexeme(l) =>
    if (e.stack == []) {
      expect_end(l);
    } else {
      w_lexeme(k, e, l);
    }
  | `End as v =>
    if (e.stack == []) {
      flush(k, e);
    } else {
      expect_lend(List.hd(e.stack), v);
    }
  | `White(w) => writes(w, 0, String.length(w), k, e)
  | `Comment(`S, c) =>
    writes("//", 0, 2, writes(c, 0, String.length(c), writeb(u_nl, k)), e)
  | `Comment(`M, c) =>
    writes(
      "/*",
      0,
      2,
      writes(c, 0, String.length(c), writes("*/", 0, 2, k)),
      e,
    )
  | `Await => `Ok;

let rec encode_loop = e => {
  e.k = encode_(encode_loop);
  `Ok;
};
let rec encode_json = e =>
  fun /* first [k] to start with [`Os] or [`As]. */
  | `Lexeme((`Null | `Bool(_) | `Float(_) | `String(_) | `As | `Os) as l) =>
    w_value(false, l, encode_loop, e)
  | (`End | `Lexeme(_)) as v => expect_json(v)
  | (`White(_) | `Comment(_)) as v =>
    encode_(
      e => {
        e.k = encode_json;
        `Ok;
      },
      e,
      v,
    )
  | `Await => `Ok;

let encoder = (~minify=true, dst) => {
  let (o, o_pos, o_max) =
    switch (dst) {
    | `Manual => (Bytes.empty, 1, 0) /* implies [o_rem e = 0]. */
    | `Buffer(_)
    | `Channel(_) => (Bytes.create(io_buffer_size), 0, io_buffer_size - 1)
    };

  {
    dst: (dst :> dst),
    minify,
    o,
    o_pos,
    o_max,
    buf: Buffer.create(30),
    stack: [],
    nest: 0,
    next_name: false,
    last_start: false,
    k: encode_json,
  };
};

let encode = (e, v) => e.k(e, (v :> [ encode | uncut]));
let encoder_dst = e => e.dst;
let encoder_minify = e => e.minify;

/* Manual */

module Manual = {
  let src = d => Uutf.Manual.src(d.u);
  let dst = dst;
  let dst_rem = o_rem;
};

/* Uncut */

module Uncut = {
  let decode = decode_uncut;
  let pp_decode = pp_decode;
  let encode = (e, v) => e.k(e, (v :> [ encode | uncut]));
};

/*---------------------------------------------------------------------------
   Copyright (c) 2012 The jsonm programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/
