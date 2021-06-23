let rec deleteFile = (url, name) =>
  Node.Fs.existsSync(url) ?
    Node.Fs.readdirSync(url)
    |> Js.Array.forEach(file => {
         let currentPath = Node.Path.join2(url, file);

         NodeExtend.statSync(currentPath)##isDirectory() ?
           deleteFile(currentPath, name) :
           file |> Js.String.indexOf(name) > (-1) ?
             Node.Fs.unlinkSync(currentPath) : ();
       }) :
    ();

let rec deleteDirAllFile = url =>
  Node.Fs.existsSync(url) ?
    Node.Fs.readdirSync(url)
    |> Js.Array.forEach(file => {
         let currentPath = Node.Path.join2(url, file);

         NodeExtend.statSync(currentPath)##isDirectory() ?
           deleteDirAllFile(currentPath) : Node.Fs.unlinkSync(currentPath);
       }) :
    ();

let mkdirSyncIfNotExist = dirPath =>
  Node.Fs.existsSync(dirPath) ? () : NodeExtend.mkdirSync(dirPath);

let writeAllFileChunk = (filePath, chunkPath, (totalCount, hash)) => {
  for (x in 0 to totalCount - 1) {
    let fileChunkPath = {j|$chunkPath$hash-$x|j};

    let result = Node.Fs.readFileSync(fileChunkPath, `binary);

    NodeExtend.appendFileSync(filePath, result, "binary");
    Node.Fs.unlinkSync(fileChunkPath);
  };

  Node.Fs.rmdirSync(chunkPath);
};
let getExtName = fileName => {
  let lastIndex = fileName |> Js.String.lastIndexOf(".");

  lastIndex === (-1) ?
    "" : fileName |> Js.String.substringToEnd(~from=lastIndex);
};
[%raw
{|
var base = new String(fileName).substring(fileName.lastIndexOf('/') + 1);
    if(base.lastIndexOf(".") !== -1)
        base = base.substring(0, base.lastIndexOf("."));
   return base;
  |}
];
[@val] external _getBaseName: string => string ="base";

let getBaseName =
  fileName => _getBaseName(fileName);

let _getPathAndFileName = (filePath, regex) =>
  switch (regex |> Js.Re.exec(filePath)) {
  | None => (filePath |> Js.Undefined.return, "")
  | Some(result) =>
    let resultArr = Js.Re.matches(result);
    (resultArr[1] |> Js.Undefined.return, resultArr[2]);
  };

let getFolderPathAndFileName = filePath =>
  _getPathAndFileName(filePath, [%re {|/^(.*[\/])?(\w+\.\w+)$/|}]);

let getTextureFolderPathAndName = filePath =>
  _getPathAndFileName(filePath, [%re {|/^(.*[\/])?(\w+)$/|}]);

// let removePathPostfix = filePath =>
//   switch ([%re {|/^(.*)[\/]$/|}] |> Js.Re.exec(filePath)) {
//   | None => filePath
//   | Some(result) =>
//     let resultArr = Js.Re.captures(result);
//     resultArr[1];
//   };

let buildFileTotalName = (baseName, extName) => baseName ++ extName;
module Unix = {
  let dirname: option(string) = [%bs.node __dirname];
  let dirnameOrDot = Js.Option.getWithDefault(".", dirname);

  [@bs.val] [@bs.module "fs"] [@warning "-103"]
  external readFileSync:
    (string, [ | `hex | `utf8 | `ascii]) => string =
    "readFileSync";

  [@bs.val] [@bs.module "fs"]
  external writeFileSync:
    (string, string, [ | `hex | `utf8 | `ascii]) => unit =
    "writeFileSync";

  [@bs.val] [@bs.module "fs"]
  external readFile:
    (
      string,
      [ | `hex | `utf8 | `ascii],
      (Js.null(Js.Exn.t), string) => unit
    ) =>
    unit =
    "readFile";

  [@bs.val] [@bs.module "fs"]
  external writeFile:
    (
      string,
      string,
      [ | `hex | `utf8 | `ascii],
      Js.null(Js.Exn.t) => unit
    ) =>
    unit =
    "writeFile";
};
