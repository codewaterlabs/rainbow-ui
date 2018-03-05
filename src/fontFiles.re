type t = {
  bin: array(int),
  image: Reasongl.Gl.imageT
};

let request = (fontName, sheet, callback) =>
  AjaxLoader.(
    loadFiles(
      [
        BinaryAsset("fonts/" ++ fontName ++ ".bin"),
        ImageAsset("fonts/" ++ fontName ++ "_" ++ sheet ++ ".png")
      ],
      results =>
      switch (results[0], results[1]) {
      | (AjaxBinaryResult(buffer), AjaxImageResult(image)) =>
        let fontFiles = {bin: buffer, image};
        callback(fontFiles);
      | _ => failwith("Font loading failed")
      }
    )
  );
