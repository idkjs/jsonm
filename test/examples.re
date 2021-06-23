// /* Examples form the documentation (see also jtree.ml), this code is in public
//    domain.  */

// /* Trip */

// let trip =
//     (
//       ~encoding=?,
//       ~minify=?,
//       src: [ | `Channel(in_channel) | `String(string)],
//       dst: [ | `Channel(out_channel) | `Buffer(Buffer.t)],
//     ) => {
//   let rec loop = (d, e) =>
//     switch (Jsonm.decode(d)) {
//     | `Lexeme(_) as v =>
//       ignore(Jsonm.encode(e, v));
//       loop(d, e);
//     | `End =>
//       ignore(Jsonm.encode(e, `End));
//       `Ok;
//     | `Error(err) => `Error((Jsonm.decoded_range(d), err))
//     | `Await => assert(false)
//     };

//   let d = Jsonm.decoder(~encoding?, src);
//   let e = Jsonm.encoder(~minify?, dst);
//   loop(d, e);
// };

// let trip_fd =
//     (~encoding=?, ~minify=?, fdi, fdo) => {
//   let rec encode = (fd, s, e, v) =>
//     switch (Jsonm.encode(e, v)) {
//     | `Ok => ()
//     | `Partial =>
//       let rec unix_write = (fd, s, j, l) => {
//         let rec write = (fd, s, j, l) =>
//         try(NodeUtils.Unix.writeFile(fd, s, j, l)) {
// | Js.Exn.Error(obj) =>
//   switch (Js.Exn.message(obj)) {
//   | Some(m) => Js.log("Caught a JS exception! Message: " ++ m)
//   | None => write(fd, s, j, l)
//   }
// };
//           // try(NodeUtils.Unix.writeFile(fd, s, j, l)) {
//           // | NodeUtils.Unix.Unix_error(NodeUtils.Unix.EINTR, _, _) =>
//           //   write(fd, s, j, l)
//           // };

//         let wc = write(fd, s, j, l);
//         if (wc < l) {
//           unix_write(fd, s, j + wc, l - wc);
//         } else {
//           ();
//         };
//       };

//       unix_write(fd, s, 0, Bytes.length(s) - Jsonm.Manual.dst_rem(e));
//       Jsonm.Manual.dst(e, s, 0, Bytes.length(s));
//       encode(fd, s, e, `Await);
//     };

//   let rec loop = (fdi, fdo, ds, es, d, e) =>
//     switch (Jsonm.decode(d)) {
//     | `Lexeme(_) as v =>
//       encode(fdo, es, e, v);
//       loop(fdi, fdo, ds, es, d, e);
//     | `End =>
//       encode(fdo, es, e, `End);
//       `Ok;
//     | `Error(err) => `Error((Jsonm.decoded_range(d), err))
//     | `Await =>
//       let rec unix_read = (fd, s, j, l) =>
//         try(NodeUtils.Unix.read(fd, s, j, l)) {
//         | NodeUtils.Unix.Unix_error(NodeUtils.Unix.EINTR, _, _) =>
//           unix_read(fd, s, j, l)
//         };

//       let rc = unix_read(fdi, ds, 0, Bytes.length(ds));
//       Jsonm.Manual.src(d, ds, 0, rc);
//       loop(fdi, fdo, ds, es, d, e);
//     };

//   let ds = Bytes.create(65536) /* UNIX_BUFFER_SIZE in 4.0.0 */;
//   let es = Bytes.create(65536) /* UNIX_BUFFER_SIZE in 4.0.0 */;
//   let d = Jsonm.decoder(~encoding?, `Manual);
//   let e = Jsonm.encoder(~minify?, `Manual);
//   Jsonm.Manual.dst(e, es, 0, Bytes.length(es));
//   loop(fdi, fdo, ds, es, d, e);
// };

// /* Member selection */

// let memsel =
//     (~encoding=?, names, src: [ | `Channel(in_channel) | `String(string)]) => {
//   let rec loop = (acc, names, d) =>
//     switch (Jsonm.decode(d)) {
//     | `Lexeme(`Name(n)) when List.mem(n, names) =>
//       switch (Jsonm.decode(d)) {
//       | `Lexeme(`String(s)) => loop([s, ...acc], names, d)
//       | _ => loop(acc, names, d)
//       }
//     | `Lexeme(_)
//     | `Error(_) => loop(acc, names, d)
//     | `End => List.rev(acc)
//     | `Await => assert(false)
//     };

//   loop([], names, Jsonm.decoder(~encoding?, src));
// };
