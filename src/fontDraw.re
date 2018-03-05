open Gpu;

type t = {
  mutable part: FontText.part,
  blockInfo: FontText.blockInfo,
  vertices: VertexBuffer.t,
  indices: IndexBuffer.t,
  uModel: Scene.sceneUniform,
  vo: Scene.sceneVertexObject,
  textures: list(Gpu.Texture.t),
  aspect: float,
  fontLayout: FontText.FontLayout.t,
  multicolor: bool
};

let vertexSource = fontDraw => {
  let (attribs, varyings, main) =
    if (fontDraw.multicolor) {
      (
        "attribute vec3 color;\n",
        "varying vec3 vColor;\n",
        "vColor = color;\n"
      );
    } else {
      ("", "", "");
    };
  {|
    precision mediump float;
    attribute vec2 position;
    attribute vec2 uv;
    |}
  ++ attribs
  ++ {|
    attribute float size;

    uniform mat3 model;
    uniform mat3 layout;
    uniform vec2 pixelSize;

    varying vec2 vUv;
    varying float smoothFactor;
    |}
  ++ varyings
  ++ {|

    void main() {
        vUv = uv;
      |}
  ++ main
  ++ {|
        // Bit trying and failing to get this somewhat right
        // Can probably be calibrated better
        smoothFactor = model[0][0] / pixelSize.x / size * 600.0;
        vec2 pos = vec3(vec3(position, 1.0) * model * layout).xy;
        gl_Position = vec4(pos, 0.0, 1.0);
    }
  |};
};

/* https://github.com/libgdx/libgdx/wiki/Distance-field-fonts */
let fragmentSource = fontDraw => {
  let (varyings, uniforms, colorGl) =
    if (fontDraw.multicolor) {
      ("varying vec3 vColor;\n", "", "vColor");
    } else {
      ("", "uniform vec3 color;\n", "color");
    };
  {|
    #ifdef GL_OES_standard_derivatives
    #extension GL_OES_standard_derivatives : enable
    #endif
    precision mediump float;
    uniform sampler2D map;
    |}
  ++ uniforms
  ++ {|
    uniform float opacity;
    varying vec2 vUv;
    |}
  ++ varyings
  ++ {|
    varying float smoothFactor;

    float aastep(float value) {
      #ifdef GL_OES_standard_derivatives
        float afwidth = length(vec2(dFdx(value), dFdy(value))) * 0.70710678118654757;
      #else
        float afwidth = (1.0 / 32.0) * (1.4142135623730951 / (2.0 * gl_FragCoord.w));
      #endif
      afwidth = afwidth * smoothFactor;
      return smoothstep(0.5 - afwidth, 0.5 + afwidth, value);
    }

    void main() {
        float discardLimit = 0.0001;
        vec4 texColor = 1.0 - texture2D(map, vUv);
        float alpha = aastep(texColor.x);
        vec3 c = |}
  ++ colorGl
  ++ {| * alpha;
        gl_FragColor = vec4(c, opacity * alpha);
        if (alpha < discardLimit) {
            discard;
        }
    }
  |};
};

let msdfVertexSource =
  String.trim(
    {|#version 300 es
    precision mediump float;
    attribute vec2 position;
    attribute vec2 uv;
    varying vec2 vUv;

    //uniform mat3 model;

    void main() {
        vUv = uv;
        //vec2 pos = vec3(model * vec3(position, 1.0)).xy;
        vec2 pos = position;
        gl_Position = vec4((pos - vec2(40.0, 0.0)) / 50., 0.0, 1.0);
    }
|}
  );

let msdfFragmentSource =
  String.trim(
    {|#version 300 es
    #ifdef GL_OES_standard_derivatives
    #extension GL_OES_standard_derivatives : enable
    #endif
    precision mediump float;
    uniform sampler2D map;
    varying vec2 vUv;

    float median(float r, float g, float b) {
        return max(min(r, g), min(max(r, g), b));
    }

    void main() {
        float opacity = 0.5;
        vec3 color = vec3(0.2, 0.5, 0.8);
        vec3 sample = 1.0 - texture2D(map, vUv).rgb;
        float sigDist = median(sample.r, sample.g, sample.b) - 0.5;
        float alpha = clamp(sigDist/fwidth(sigDist) + 0.5, 0.0, 1.0);
        gl_FragColor = vec4(color.xyz, alpha * opacity);
    }
|}
  );

module StoreSpec = {
  type hash = {multicolor: bool};
  type progType = t;
  let getHash = (fontDraw: t) : hash => {multicolor: fontDraw.multicolor};
  let createProgram = (fontDraw: t) => {
    let requiredUniforms =
      switch fontDraw.multicolor {
      | false => [
          ("model", Gpu.GlType.Mat3f),
          ("color", Gpu.GlType.Vec3f),
          ("opacity", Gpu.GlType.Float)
        ]
      | true => [("model", Gpu.GlType.Mat3f), ("opacity", Gpu.GlType.Float)]
      };
    let attribs =
      if (fontDraw.multicolor) {
        [
          VertexAttrib.make("position", GlType.Vec2f),
          VertexAttrib.make("uv", GlType.Vec2f),
          VertexAttrib.make("size", GlType.Float),
          VertexAttrib.make("color", GlType.Vec3f)
        ];
      } else {
        [
          VertexAttrib.make("position", GlType.Vec2f),
          VertexAttrib.make("uv", GlType.Vec2f),
          VertexAttrib.make("size", GlType.Float)
        ];
      };
    /* Todo: Number of textures (fonts or fonts with several) needs to be in hash,
       and reflected in required textures */
    Scene.makeProgram(
      ~vertShader=Gpu.Shader.make(vertexSource(fontDraw)),
      ~fragShader=Gpu.Shader.make(fragmentSource(fontDraw)),
      ~requiredUniforms,
      ~attribs,
      ~requiredTextures=[("map", false)],
      ()
    );
  };
  let tblSize = 2;
};

module Programs = ProgramStore.Make(StoreSpec);

let makeText =
    (part: FontText.part, fontLayout: FontText.FontLayout.t, ~height=0.5, ()) => {
  let blockInfo = FontText.getPartInfo(part);
  let textures =
    List.map(
      font => FontStore.getTexture(fontLayout.store, font),
      blockInfo.fonts
    );
  let multicolor = List.length(blockInfo.colors) > 1 ? true : false;
  let attribs =
    if (multicolor) {
      [
        ("position", GlType.Vec2f),
        ("uv", GlType.Vec2f),
        ("size", GlType.Float),
        ("color", GlType.Vec3f)
      ];
    } else {
      [
        ("position", GlType.Vec2f),
        ("uv", GlType.Vec2f),
        ("size", GlType.Float)
      ];
    };
  let vertices = VertexBuffer.make([||], attribs, DynamicDraw);
  let indices = IndexBuffer.make([||], DynamicDraw);
  let uModel = Scene.UMat3f.id();
  let aspect = 1.0 /. height;
  let vo = Scene.SceneVO.make(vertices, Some(indices));
  {
    part,
    blockInfo,
    vertices,
    indices,
    uModel,
    vo,
    textures,
    aspect,
    fontLayout,
    multicolor
  };
};

let makeSimpleText =
    (
      text,
      font,
      fontLayout,
      ~height=0.2,
      ~numLines=1,
      ~align=FontText.Left,
      ~color=Color.fromFloats(1.0, 1.0, 1.0),
      ()
    ) =>
  makeText(
    FontText.block(~height, ~font, ~color, ~align, [FontText.text(text)]),
    fontLayout,
    ~height=height *. float_of_int(numLines),
    ()
  );

let updateNode = (fontDraw: t, node: Scene.node('s)) => {
  node.loading = true;
  /* Callback will trigger in function if font is already loaded */
  FontStore.requestMultiple(
    fontDraw.fontLayout.store,
    fontDraw.blockInfo.fonts,
    _store => {
      let (vertices, yLineEnd) =
        FontText.FontLayout.layoutVertices(
          fontDraw.fontLayout,
          fontDraw.part,
          fontDraw.multicolor
        );
      VertexBuffer.setDataT(fontDraw.vertices, vertices);
      let numVertices =
        fontDraw.multicolor ?
          FontText.FontLayout.numColorVertices :
          FontText.FontLayout.numVertices;
      IndexBuffer.setDataT(
        fontDraw.indices,
        IndexBuffer.makeQuadsData(Array.length(vertices) / numVertices)
      );
      /* Text layout on y axis goes from 0.0 downwards,
         this vertically aligns in (roughly, to fix) middle */
      let yTrans = yLineEnd *. (-0.5);
      let modelMat =
        Data.Mat3.matmul(
          Data.Mat3.scale(1.0, fontDraw.aspect),
          Data.Mat3.trans(0.0, yTrans)
        );
      Uniform.setMat3f(fontDraw.uModel.uniform, modelMat);
      node.loading = false;
    }
  );
};

let makeNode =
    (
      fontDraw: t,
      ~key=?,
      ~cls="fontDraw",
      ~opacity=1.0,
      ~hidden=false,
      ~margin=?,
      ()
    ) => {
  let uniforms =
    switch fontDraw.multicolor {
    | false =>
      let color =
        switch fontDraw.blockInfo.colors {
        | [color] => color
        | _ => failwith("Could not find font color")
        };
      [
        ("model", fontDraw.uModel),
        ("color", Scene.UVec3f.vec(Color.toVec3(color))),
        ("opacity", Scene.UFloat.make(opacity))
      ];
    | true => [
        ("model", fontDraw.uModel),
        ("opacity", Scene.UFloat.make(opacity))
      ]
    };
  let textures =
    List.mapi(
      (i, texture) =>
        if (i == 0) {
          ("map", Scene.SceneTex.tex(texture));
        } else {
          ("map" ++ string_of_int(i + 1), Scene.SceneTex.tex(texture));
        },
      fontDraw.textures
    );
  let node =
    Scene.makeNode(
      ~key?,
      ~cls,
      ~program=Programs.getProgram(fontDraw),
      ~textures,
      ~vo=fontDraw.vo,
      ~uniforms,
      ~pixelSizeUniform=true,
      ~transparent=true,
      ~loading=true,
      ~size=Scene.Aspect(fontDraw.aspect),
      ~margin?,
      ~hidden,
      ()
    );
  updateNode(fontDraw, node);
  node;
};

let makePartNode =
    (
      block,
      fontLayout,
      ~height=0.5,
      ~key=?,
      ~cls=?,
      ~opacity=1.0,
      ~hidden=false,
      ~margin=?,
      ()
    ) =>
  makeNode(
    makeText(block, fontLayout, ~height, ()),
    ~key?,
    ~cls?,
    ~opacity,
    ~hidden,
    ~margin?,
    ()
  );

let makeSimpleNode =
    (
      text,
      font,
      fontLayout,
      ~height=0.2,
      ~numLines=1,
      ~align=FontText.Left,
      ~color=Color.fromFloats(1.0, 1.0, 1.0),
      ~key=?,
      ~cls=?,
      ~opacity=1.0,
      ~hidden=false,
      ()
    ) =>
  makeNode(
    makeSimpleText(
      text,
      font,
      fontLayout,
      ~height,
      ~numLines,
      ~align,
      ~color,
      ()
    ),
    ~key?,
    ~cls?,
    ~opacity,
    ~hidden,
    ()
  );
