type t = {
  fonts: Hashtbl.t(string, SdfFont.BMFont.bmFont),
  images: Hashtbl.t(string, Reasongl.Gl.imageT),
  textures: Hashtbl.t(string, Gpu.Texture.t),
  loading: Hashtbl.t(string, bool),
  onLoads:
    Hashtbl.t(
      string,
      list((SdfFont.BMFont.bmFont, Reasongl.Gl.imageT) => unit)
    )
};

let make = (~initSize=2, ()) => {
  fonts: Hashtbl.create(initSize),
  images: Hashtbl.create(initSize),
  textures: Hashtbl.create(initSize),
  loading: Hashtbl.create(initSize),
  onLoads: Hashtbl.create(initSize)
};

let getTexture = (store, fontName) =>
  if (Hashtbl.mem(store.textures, fontName)) {
    Hashtbl.find(store.textures, fontName);
  } else {
    open Gpu;
    let texture =
      Texture.make(Texture.EmptyTexture, Texture.RGBA, Texture.LinearFilter);
    Hashtbl.add(store.textures, fontName, texture);
    texture;
  };

let request = (store, fontName, onLoad) =>
  if (Hashtbl.mem(store.fonts, fontName)) {
    /* Already loaded */
    onLoad(
      Hashtbl.find(store.fonts, fontName),
      Hashtbl.find(store.images, fontName)
    );
  } else if (Hashtbl.mem(store.loading, fontName)) {
    /* Already loading */
    let onLoads = Hashtbl.find(store.onLoads, fontName);
    Hashtbl.replace(store.onLoads, fontName, [onLoad, ...onLoads]);
  } else {
    Hashtbl.add(store.loading, fontName, true);
    Hashtbl.add(store.onLoads, fontName, [onLoad]);
    FontFiles.request(
      fontName,
      "sheet0",
      fontFiles => {
        let bmFont = SdfFont.BMFont.parse(fontFiles.bin);
        let texture = getTexture(store, fontName);
        Gpu.Texture.setDataT(
          texture,
          Gpu.Texture.ImageTexture(fontFiles.image)
        );
        Hashtbl.add(store.fonts, fontName, bmFont);
        Hashtbl.add(store.images, fontName, fontFiles.image);
        let onLoads = Hashtbl.find(store.onLoads, fontName);
        Hashtbl.remove(store.onLoads, fontName);
        List.iter(onLoad => onLoad(bmFont, fontFiles.image), onLoads);
      }
    );
  };

let requestMultiple = (store, fonts, onLoad) => {
  let remaining = ref(List.length(fonts));
  List.iter(
    font =>
      request(
        store,
        font,
        (_font, _image) => {
          remaining := remaining^ - 1;
          if (remaining^ == 0) {
            onLoad(store);
          };
        }
      ),
    fonts
  );
};
