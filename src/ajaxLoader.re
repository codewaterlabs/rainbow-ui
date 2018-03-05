type remoteAsset =
  | BinaryAsset(string)
  | ImageAsset(string)
  | TextAsset(string);

type ajaxResult =
  | AjaxImageResult(Reasongl.Gl.imageT)
  | AjaxBinaryResult(array(int))
  | AjaxTextResult(string)
  | NoResult;

[@bs.new]
external uint8arrayAsIntArray : Reasongl.arrayBufferT => array(int) =
  "Uint8Array";

let loadFiles = (assets, cb) => {
  let totalFiles = List.length(assets);
  let completedFiles = ref(0);
  let results = Array.make(totalFiles, NoResult);
  List.iteri(
    (i, asset) =>
      switch asset {
      | BinaryAsset(fileName) =>
        let req = Reasongl.makeXMLHttpRequest();
        Reasongl.setResponseType(req, "arraybuffer");
        Reasongl.openFile(
          req,
          ~kind="GET",
          ~filename="assets/" ++ fileName,
          ~whatIsThis=Js.true_
        );
        Reasongl.onreadystatechange(req, () =>
          if (Reasongl.getReadyState(req) === 4
              && (
                Reasongl.getStatus(req) === 200
                || Reasongl.getStatus(req) === 0
              )) {
            /*results(remoteAsset) = new AjaxBinaryResult(new BinaryBuffer(new Uint8Array(arrayBuffer)))*/
            results[i] =
              AjaxBinaryResult(
                uint8arrayAsIntArray(Reasongl.getResponse(req))
              );
            completedFiles := completedFiles^ + 1;
            if (completedFiles^ == totalFiles) {
              cb(results);
            };
          }
        );
        Reasongl.sendRequest(req, Js.null);
      | ImageAsset(fileName) =>
        Reasongl.Gl.loadImage(
          ~filename="assets/" ++ fileName,
          ~callback=
            image =>
              switch image {
              | Some(image) =>
                results[i] = AjaxImageResult(image);
                completedFiles := completedFiles^ + 1;
                if (completedFiles^ == totalFiles) {
                  cb(results);
                };
              | None => failwith("Could not load image")
              },
          ()
        )
      | TextAsset(fileName) =>
        Reasongl.Gl.File.readFile(
          ~filename="assets/" ++ fileName,
          ~cb=rawFile => {
            results[i] = AjaxTextResult(rawFile);
            completedFiles := completedFiles^ + 1;
            if (completedFiles^ == totalFiles) {
              cb(results);
            };
          }
        )
      },
    assets
  );
};
