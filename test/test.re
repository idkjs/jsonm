/*---------------------------------------------------------------------------
   Copyright (c) 2012 The jsonm programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/

let str = Format.sprintf;
let log = f => Format.kfprintf (ppf => Format.fprintf(ppf,"@?"), Format.std_formatter, f)
let fail = fmt => {
  let fail = _ => failwith(Format.flush_str_formatter());
  Format.kfprintf(fail, Format.str_formatter, fmt);
};

let encoder_invalid = () => {
  log("Invalid encodes.\n");
  let rec encode_seq = e =>
    fun
    | [v, ...vs] => {
        ignore(Jsonm.Uncut.encode(e, v));
        encode_seq(e, vs);
      }
    | [] => ();

  let seq = (~invalid, s) => {
    let test = (~minify) => {
      let e = Jsonm.encoder(~minify, `Buffer(Buffer.create(256)));
      try(
        {
          encode_seq(e, s);
          assert(!invalid);
        }
      ) {
      | Invalid_argument(_) as e =>
        if (invalid) {
          ();
        } else {
          raise(e);
        }
      };
    };

    test(~minify=true);
    test(~minify=false);
  };

  seq(~invalid=false, [`Lexeme(`Null)]);
  seq(~invalid=false, [`Lexeme(`Bool(true))]);
  seq(~invalid=false, [`Lexeme(`Bool(false))]);
  seq(~invalid=false, [`Lexeme(`Float(1.0))]);
  seq(~invalid=false, [`Lexeme(`String("bla"))]);
  seq(~invalid=true, [`Lexeme(`Ae)]);
  seq(~invalid=true, [`Lexeme(`Oe)]);
  seq(~invalid=true, [`Lexeme(`Name("b"))]);
  seq(~invalid=true, [`White("    "), `Lexeme(`Oe)]);
  seq(~invalid=true, [`Lexeme(`Os), `Lexeme(`Ae)]);
  seq(~invalid=true, [`Comment((`S, "bla")), `Lexeme(`Os), `Lexeme(`Ae)]);
  seq(~invalid=true, [`Lexeme(`Os), `Lexeme(`Null)]);
  seq(
    ~invalid=true,
    [`Lexeme(`Os), `Lexeme(`Name("b")), `Lexeme(`Name("b"))],
  );
  seq(~invalid=true, [`Lexeme(`Os), `Lexeme(`Name("b")), `Lexeme(`Ae)]);
  seq(~invalid=true, [`Lexeme(`Os), `Lexeme(`Name("b")), `Lexeme(`Oe)]);
  seq(~invalid=true, [`Lexeme(`Os), `Lexeme(`Name("b")), `Lexeme(`Ae)]);
  seq(
    ~invalid=true,
    [`Lexeme(`Os), `Lexeme(`Name("b")), `Lexeme(`Null), `Lexeme(`Null)],
  );
  seq(
    ~invalid=true,
    [
      `Lexeme(`Os),
      `Lexeme(`Name("b")),
      `Lexeme(`Null),
      `Lexeme(`Name("c")),
      `Lexeme(`Ae),
    ],
  );
  seq(~invalid=true, [`Lexeme(`As), `Lexeme(`Oe)]);
  seq(~invalid=true, [`Lexeme(`As), `Lexeme(`Name("b"))]);
  seq(
    ~invalid=true,
    [`Lexeme(`As), `Lexeme(`Null), `Lexeme(`Name("b"))],
  );
  seq(~invalid=true, [`Lexeme(`As), `Lexeme(`Ae), `Lexeme(`As)]);
  seq(~invalid=true, [`Lexeme(`As), `Lexeme(`Ae), `Lexeme(`Null)]);
  seq(~invalid=true, [`Lexeme(`As), `Lexeme(`Ae), `End, `Lexeme(`As)]);
  seq(~invalid=true, [`Lexeme(`Os), `Lexeme(`Oe), `Lexeme(`Os)]);
  seq(~invalid=true, [`Lexeme(`Os), `Lexeme(`Oe), `Lexeme(`Null)]);
  seq(~invalid=true, [`Lexeme(`Os), `Lexeme(`Oe), `End, `Lexeme(`Os)]);
  ();
};

let encoder_escapes = () => {
  log("Encoder escapes.\n");
  let encode = (ascii, sascii) => {
    let b = Buffer.create(10);
    let e = Jsonm.encoder(`Buffer(b));
    let enc = v => ignore(Jsonm.encode(e, `Lexeme(v)));
    List.iter(
      enc,
      [`As, `String(Printf.sprintf("%c", Char.chr(ascii))), `Ae],
    );
    ignore(Jsonm.encode(e, `End));
    let json = Buffer.contents(b);
    let exp = str("[\"%s\"]", sascii);
    if (json != exp) {
      fail("found: %s exp: %s", json, exp);
    };
  };

  encode(0x22, "\\\"");
  encode(0x5C, "\\\\");
  for (i in 0x00 to 0x1F) {
    if (i == 0x0A) {
      encode(i, "\\n");
    } else {
      encode(i, str("\\u00%02X", i));
    };
  };
  ();
};

let decoder_encoding_guess = () => {
  log("Decoder encoding guesses.\n");
  let test = (enc, s) => {
    let d = Jsonm.decoder(`String(s));
    let enc' = {
      ignore(Jsonm.decode(d));
      Jsonm.decoder_encoding(d);
    };
    if (enc' != enc) {
      fail(
        "found: %s exp: %s",
        Uutf.encoding_to_string(enc'),
        Uutf.encoding_to_string(enc),
      );
    };
  };

  test(`UTF_8, "[]");
  test(`UTF_8, "{}");
  test(`UTF_16BE, "\000[\000]");
  test(`UTF_16BE, "\000{\000}");
  test(`UTF_16LE, "[\000]\000");
  test(`UTF_16LE, "{\000}\000");
  ();
};

let test_decode = (fnd, exp) =>
  if (fnd != exp) {
    fail(
      "found: %a expected: %a",
      Jsonm.Uncut.pp_decode,
      fnd,
      Jsonm.Uncut.pp_decode,
      exp,
    );
  };

let test_seq = (decode, src, seq) => {
  let d = Jsonm.decoder(`String(src));
  let rec loop = d =>
    fun
    | [] => {
        if (decode(d) != `End) {
          fail("decoder not at the `End");
        };
        ();
      }
    | [v, ...vs] => {
        test_decode(decode(d), v);
        loop(d, vs);
      };

  loop(d, seq);
};

let arr = seq => [`Lexeme(`As)] @ seq @ [`Lexeme(`Ae), `End, `End, `End];

let decoder_comments = () => {
  log("Decoder comments.\n");
  let test = ((s, c), src) =>
    test_seq(Jsonm.Uncut.decode, src, arr([`Comment((s, c))]));
  let test_eoi = (v, src) =>
    test_seq(
      Jsonm.Uncut.decode,
      src,
      [`Lexeme(`As), `Lexeme(`Ae), v, `End],
    );

  test((`M, "bla"), "[/*bla*/]");
  test((`M, "b*"), "[/*b**/]");
  test((`M, "b** /"), "[/*b** /*/]");
  test((`M, "b** /"), "[/*b** /*/]");
  test((`M, "b***\n/"), "[/*b***\n/*/]");
  test((`S, "abcd"), "[//abcd\n]");
  test_eoi(`Comment((`S, "abcd")), "[]//abcd");
  test_eoi(`Comment((`S, "abcd///* ")), "[]//abcd///* ");
  test_eoi(`Comment((`M, " abcd ")), "[]/* abcd */");
  test_eoi(`Comment((`M, " abcd ")), "[]/* abcd */");
  test_eoi(`Error(`Unclosed(`Comment)), "[]/* abcd ");
  test_eoi(`Error(`Expected(`Comment)), "[]/");
  ();
};

let decoder_escapes = () => {
  log("Decoder escapes.\n");
  let test = (str, src) =>
    test_seq(Jsonm.decode, src, arr([`Lexeme(`String(str))]));
  let test_ill = (ill, str, src) =>
    test_seq(
      Jsonm.decode,
      src,
      arr([`Error(`Illegal_escape(ill)), `Lexeme(`String(str))]),
    );

  let not_esc_uchar = u => `Not_esc_uchar(Uchar.of_int(u));
  let not_hex_uchar = u => `Not_hex_uchar(Uchar.of_int(u));
  let s = s => Printf.sprintf("[\"%s\"]", s);
  test("<\">", s("<\\\">"));
  test("<\\>", s("<\\\\>"));
  test("</>", s("<\\/>"));
  test("<\b>", s("<\\b>"));
  test("<\012>", s("<\\f>"));
  test("<\n>", s("<\\n>"));
  test("<\r>", s("<\\r>"));
  test("<\t>", s("<\\t>"));
  test("<𝄞><水>", s("<\\uD834\\uDd1E><\\u6C34>"));
  test_ill(not_esc_uchar(0x61), "<�>", s("<\\a>"));
  test_ill(not_esc_uchar(0x31), "<�>", s("<\\1>"));
  test_ill(not_esc_uchar(0xFFFD), "<�>", s("<\\�>"));
  test_ill(not_hex_uchar(0x47), "<�AF1>", s("<\\uGAF1>"));
  test_ill(not_hex_uchar(0x47), "<�F1>", s("<\\uAGF1>"));
  test_ill(not_hex_uchar(0x67), "<�1>", s("<\\uAFg1>"));
  test_ill(not_hex_uchar(0x67), "<�>", s("<\\uAF1g>"));
  test_ill(`Not_lo_surrogate(0x6C34), "<�>", s("<\\uD834\\u6C34>"));
  test_ill(`Lone_hi_surrogate(0xD834), "<�bla>", s("<\\uD834bla>"));
  test_ill(`Lone_lo_surrogate(0xDD1E), "<�bla>", s("<\\uDd1Ebla>"));
  test_ill(`Lone_hi_surrogate(0xD834), "<�\nf>", s("<\\uD834\\nf>"));
  ();
};

let decoder_strings = () => {
  log("Decoder strings.\n");
  test_seq(Jsonm.decode, "\"\"", [`Lexeme(`String(""))]);
  test_seq(Jsonm.decode, "\"heyho\"", [`Lexeme(`String("heyho"))]);
  test_seq(
    Jsonm.decode,
    "[\"blibla\"]",
    arr([`Lexeme(`String("blibla"))]),
  );
  test_seq(
    Jsonm.decode,
    "[\"bli\nbla\"]",
    arr([
      `Error(`Illegal_string_uchar(Uchar.of_int(0x0A))),
      `Lexeme(`String("bli�bla")),
    ]),
  );
  test_seq(
    Jsonm.decode,
    "[\"blabla",
    [`Lexeme(`As), `Error(`Unclosed(`Comment)), `End, `End],
  );
  ();
};

let decoder_literals = () => {
  log("Decoder literals.\n");
  test_seq(Jsonm.decode, "null", [`Lexeme(`Null)]);
  test_seq(Jsonm.decode, "true", [`Lexeme(`Bool(true))]);
  test_seq(Jsonm.decode, "false", [`Lexeme(`Bool(false))]);
  test_seq(Jsonm.decode, "[null]", arr([`Lexeme(`Null)]));
  test_seq(Jsonm.decode, "[true]", arr([`Lexeme(`Bool(true))]));
  test_seq(Jsonm.decode, "[false]", arr([`Lexeme(`Bool(false))]));
  test_seq(
    Jsonm.decode,
    "[truee]",
    arr([`Error(`Illegal_literal("truee"))]),
  );
  test_seq(
    Jsonm.decode,
    "[tru",
    [
      `Lexeme(`As),
      `Error(`Illegal_literal("tru")),
      `Error(`Unclosed(`As)),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "{\"\" : tru",
    [
      `Lexeme(`Os),
      `Lexeme(`Name("")),
      `Error(`Illegal_literal("tru")),
      `Error(`Unclosed(`Os)),
      `End,
      `End,
      `End,
    ],
  );
  ();
};

let decoder_numbers = () => {
  log("Decoder numbers.\n");
  test_seq(Jsonm.decode, "1.0", [`Lexeme(`Float(1.0))]);
  test_seq(Jsonm.decode, "-1.0", [`Lexeme(`Float(-. 1.0))]);
  test_seq(Jsonm.decode, "[1.0]", arr([`Lexeme(`Float(1.0))]));
  test_seq(Jsonm.decode, "[1e12]", arr([`Lexeme(`Float(1e12))]));
  test_seq(Jsonm.decode, "[-1e12]", arr([`Lexeme(`Float(-. 1e12))]));
  test_seq(
    Jsonm.decode,
    "[-1eee2]",
    arr([`Error(`Illegal_number("-1eee2"))]),
  );
  test_seq(
    Jsonm.decode,
    "[-1ee2",
    [
      `Lexeme(`As),
      `Error(`Illegal_number("-1ee2")),
      `Error(`Unclosed(`As)),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "{\"\" : -1ee2",
    [
      `Lexeme(`Os),
      `Lexeme(`Name("")),
      `Error(`Illegal_number("-1ee2")),
      `Error(`Unclosed(`Os)),
      `End,
      `End,
      `End,
    ],
  );
  ();
};

let decoder_arrays = () => {
  log("Decoder arrays.\n");
  test_seq(Jsonm.decode, "[]", arr([]));
  test_seq(
    Jsonm.decode,
    "[",
    [`Lexeme(`As), `Error(`Unclosed(`As)), `End, `End, `End],
  );
  test_seq(
    Jsonm.decode,
    "[null",
    [
      `Lexeme(`As),
      `Lexeme(`Null),
      `Error(`Unclosed(`As)),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "[null,",
    [
      `Lexeme(`As),
      `Lexeme(`Null),
      `Error(`Expected(`Value)),
      `Error(`Unclosed(`As)),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "[null { null]",
    [
      `Lexeme(`As),
      `Lexeme(`Null),
      `Error(`Expected(`Aval(false))),
      `Lexeme(`Ae),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "[null { null,null]",
    [
      `Lexeme(`As),
      `Lexeme(`Null),
      `Error(`Expected(`Aval(false))),
      `Lexeme(`Null),
      `Lexeme(`Ae),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "[; null]",
    [
      `Lexeme(`As),
      `Error(`Expected(`Aval(true))),
      `Lexeme(`Ae),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "[; , null]",
    [
      `Lexeme(`As),
      `Error(`Expected(`Aval(true))),
      `Lexeme(`Null),
      `Lexeme(`Ae),
      `End,
      `End,
      `End,
    ],
  );
  ();
};

let decoder_objects = () => {
  log("Decoder objects.\n");
  test_seq(
    Jsonm.decode,
    "{",
    [`Lexeme(`Os), `Error(`Unclosed(`Os)), `End, `End, `End],
  );
  test_seq(
    Jsonm.decode,
    "{null",
    [
      `Lexeme(`Os),
      `Error(`Expected(`Omem(true))),
      `Error(`Unclosed(`Os)),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "{ \"b\" ",
    [
      `Lexeme(`Os),
      `Lexeme(`Name("b")),
      `Error(`Expected(`Name_sep)),
      `Error(`Unclosed(`Os)),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "{ \"b\" : ] null]",
    [
      `Lexeme(`Os),
      `Lexeme(`Name("b")),
      `Error(`Expected(`Value)),
      `Error(`Unclosed(`Os)),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "{ \"b\" : null",
    [
      `Lexeme(`Os),
      `Lexeme(`Name("b")),
      `Lexeme(`Null),
      `Error(`Unclosed(`Os)),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "{; null}",
    [
      `Lexeme(`Os),
      `Error(`Expected(`Omem(true))),
      `Lexeme(`Oe),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "{ ill : null, \"bli\" : null}",
    [
      `Lexeme(`Os),
      `Error(`Expected(`Omem(true))),
      `Lexeme(`Name("bli")),
      `Lexeme(`Null),
      `Lexeme(`Oe),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "{ \"bli\" : null ill : null }",
    [
      `Lexeme(`Os),
      `Lexeme(`Name("bli")),
      `Lexeme(`Null),
      `Error(`Expected(`Omem(false))),
      `Lexeme(`Oe),
      `End,
      `End,
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "{ \"bli\" : null, ill : null }",
    [
      `Lexeme(`Os),
      `Lexeme(`Name("bli")),
      `Lexeme(`Null),
      `Error(`Expected(`Name)),
      `Lexeme(`Oe),
      `End,
      `End,
      `End,
    ],
  );
  ();
};

let decoder_json_text = () => {
  log("Decoder JSON text.\n");
  test_seq(
    Jsonm.decode,
    "a",
    [`Error(`Expected(`Json)), `Error(`Expected(`Json)), `End],
  );
  test_seq(Jsonm.decode, "", [`Error(`Expected(`Json)), `End]);
  test_seq(
    Jsonm.decode,
    "a : null {}",
    [
      `Error(`Expected(`Json)),
      `Error(`Expected(`Json)),
      `Lexeme(`Null),
      `Error(`Expected(`Eoi)),
      `End,
    ],
  );
  test_seq(
    Jsonm.decode,
    "a : null []",
    [
      `Error(`Expected(`Json)),
      `Error(`Expected(`Json)),
      `Lexeme(`Null),
      `Error(`Expected(`Eoi)),
      `End,
    ],
  );
  ();
};
// TODO:this test doesnt pass
let decoder_bom = () => {
  log("Decoder BOM.\n");
  let seq = [`Error(`Illegal_BOM), `Lexeme(`Os), `Lexeme(`Oe), `End];
  // test_seq(Jsonm.decode, "﻿  {}", seq);
  // test_seq(Jsonm.decode, "��\000{\000}", seq);
  // test_seq(Jsonm.decode, "��{\000}\000", seq);
  ();
};

let decoder_eoi = () => {
  log("Decoder end of input.\n");
  test_seq(Jsonm.decode, "", [`Error(`Expected(`Json))]);
  test_seq(
    Jsonm.decode,
    "{} a : null",
    [`Lexeme(`Os), `Lexeme(`Oe), `Error(`Expected(`Eoi))],
  );
  test_seq(
    Jsonm.decode,
    "[] a : null ",
    [`Lexeme(`As), `Lexeme(`Ae), `Error(`Expected(`Eoi))],
  );
  ();
};

let trip = () => {
  log("Codec round-trips.\n");
  let trip = s => {
    let b = Buffer.create(String.length(s));
    let d = Jsonm.decoder(`String(s));
    let e = Jsonm.encoder(`Buffer(b));
    let rec loop = (d, e) =>
      switch (Jsonm.decode(d)) {
      | `Lexeme(_) as v =>
        ignore(Jsonm.encode(e, v));
        loop(d, e);
      | `End as v => ignore(Jsonm.encode(e, v))
      | `Error(e) => fail("err: %a", Jsonm.pp_error, e)
      | `Await => assert(false)
      };

    loop(d, e);
    let trips = Buffer.contents(b);
    if (trips != s) {
      fail("fnd: %s@\nexp: %s@\n", trips, s);
    };
  };

  trip("null");
  trip("true");
  trip("false");
  trip("2");
  trip("0.5");
  trip("\"heyho\"");
  trip("[null,null,0.5,true,false,[true,false]]");
  trip("{\"a\":[1,2,4,5,[true,false]],\"b\":{}}");
  trip("{\"a\":[1,2,4,5,[true,{\"c\":[null]}]],\"b\":{}}");
  trip("{\"a\":[1,2,4,5,[true,{\"c\":[\"\\nbli\",5,6]}]],\"b\":{}}");
  /* Verify that integers that can be represented exactly by an OCaml float
     value [-2^53;2^53] do trip. */
  trip("[9007199254740992,-9007199254740992]");
  ();
};

let test = () => {
  Printexc.record_backtrace(true);
  encoder_invalid();
  encoder_escapes();
  decoder_encoding_guess();
  decoder_escapes();
  decoder_comments();
  decoder_literals();
  decoder_numbers();
  decoder_arrays();
  decoder_objects();
  decoder_json_text();
  decoder_bom();
  decoder_eoi();
  trip();
  log("All tests succeeded.\n");
};

let () =
  if (! Sys.interactive^) {
    test();
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
