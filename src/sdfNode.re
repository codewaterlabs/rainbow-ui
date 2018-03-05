open Gpu;

type fragCoords =
  | ByModel
  | ZeroToOne;

type sdfColor =
  | NoColor
  | SdfStaticColor(Color.t)
  | SdfDynColor(Scene.sceneUniform);

type t = {
  sdfDist: string,
  fragCoords,
  width: float,
  height: float,
  model: option(Scene.sceneUniform),
  color: sdfColor,
  colorMix: float,
  vo: option(Scene.sceneVertexObject),
  opacity: option(float),
  alphaLimit: option(float),
  lighting: Light.ProgramLight.t
};

/* Some of these are incomplete,
   should rethink some of the options here
   that are accidental */
let makeVertexSource = self =>
  switch (self.model, self.fragCoords) {
  | (Some(_model), ByModel) => {|
        precision mediump float;
        attribute vec2 position;
        uniform mat3 model;
        uniform mat3 layout;
        uniform mat3 texTrans;
        varying vec2 vPosition;
        varying vec3 vScreenPos;
        void main() {
            vec3 toModel = vec3(position, 1.0) * model;
            vPosition = position;
            // Not sure what makes sense here, it's a bit
            // of a mess. This is what is needed for the
            // side elements though
            vec2 pos = (vec3(toModel.xy, 1.0) * layout).xy;
            vScreenPos = vec3((vec3(position, 1.0) * layout).xy, 0.0);
            pos = (vec3(toModel.xy, 1.0) * texTrans).xy;
            gl_Position = vec4(pos, 0.0, 1.0);
        }
    |}
  | (Some(_model), ZeroToOne) => {|
        precision mediump float;
        attribute vec2 position;
        uniform mat3 model;
        uniform mat3 layout;
        uniform mat3 texTrans;
        varying vec2 vPosition;
        varying vec3 vScreenPos;
        void main() {
            vec3 toModel = vec3(position, 1.0) * model;
            vec2 pos = (vec3(toModel.xy, 1.0) * layout).xy;
            vScreenPos = vec3(pos, 0.0);
            vPosition = position;
            // texTrans
            pos = (vec3(position, 1.0) * texTrans).xy;
            gl_Position = vec4(pos, 0.0, 1.0);
        }
    |}
  | (None, ZeroToOne) => {|
        precision mediump float;
        attribute vec2 position;
        uniform mat3 layout;
        uniform mat3 texTrans;
        varying vec2 vPosition;
        varying vec3 vScreenPos;
        void main() {
            vPosition = (position + vec2(1.0, -1.0)) * vec2(0.5, -0.5);
            vec2 pos = (vec3(position, 1.0) * layout).xy;
            vScreenPos = vec3(pos, 0.0);
            // texTrans
            pos = (vec3(position, 1.0) * texTrans).xy;
            gl_Position = vec4(pos, 0.0, 1.0);
        }
    |}
  | (None, _) => {|
        precision mediump float;
        attribute vec2 position;
        uniform mat3 layout;
        uniform mat3 texTrans;
        varying vec2 vPosition;
        varying vec3 vScreenPos;
        void main() {
            vPosition = position;
            vec2 pos = (vec3(position, 1.0) * layout).xy;
            vScreenPos = vec3(pos, 0.0);
            // texTrans
            pos = (vec3(pos, 1.0) * texTrans).xy;
            gl_Position = vec4(pos, 0.0, 1.0);
        }
    |}
  };

let makeFragmentSource = self => {
  let fragCoords =
    switch (self.model, self.fragCoords) {
    | (Some(_model), ByModel) => "vPosition"
    | (_, ZeroToOne) => "vPosition"
    | (None, ByModel) =>
      failwith("ByModel fragCoords requested, but no model provided")
    };
  let sf = string_of_float;
  let (uniformDecls, glColor) =
    switch self.color {
    | NoColor => ("", "color")
    | SdfStaticColor(color) => (
        "",
        "mix(color, "
        ++ Color.toGlsl(color)
        ++ ", "
        ++ string_of_float(self.colorMix)
        ++ ")"
      )
    | SdfDynColor(_) => (
        "uniform vec3 uColor;\n",
        "mix(color, uColor, " ++ string_of_float(self.colorMix) ++ ")"
      )
    };
  let glAlpha =
    switch (self.alphaLimit, self.opacity) {
    | (Some(alphaLimit), Some(opacity)) =>
      "(pixelEye.z - dist < " ++ sf(alphaLimit) ++ ") ? 0.0 : " ++ sf(opacity)
    | (Some(alphaLimit), None) =>
      "(pixelEye.z - dist < " ++ sf(alphaLimit) ++ ") ? 0.0 : 1.0"
    | (None, Some(opacity)) => sf(opacity)
    | (None, None) => "1.0"
    };
  let lightDecls = Light.ProgramLight.getFragVarDecls(self.lighting);
  let lightSrc = Light.ProgramLight.getLightFunction(self.lighting);
  let source =
    {|
        precision mediump float;
        varying vec2 vPosition;
        varying vec3 vScreenPos;

        |}
    ++ uniformDecls
    ++ {|

        float epsilon = 0.00005;
        float minDist = 1.0;
        float maxDist = 5.0;
        const int marchingSteps = 30;

        |}
    ++ lightDecls
    ++ {|

        |}
    ++ lightSrc
    ++ {|

        float sdfDist(vec3 point) {
            |}
    ++ self.sdfDist
    ++ {|
        }

        vec3 estimateNormal(vec3 point) {
            return normalize(vec3(
                sdfDist(vec3(point.x + epsilon, point.y, point.z)) - sdfDist(vec3(point.x - epsilon, point.y, point.z)),
                sdfDist(vec3(point.x, point.y + epsilon, point.z)) - sdfDist(vec3(point.x, point.y - epsilon, point.z)),
                sdfDist(vec3(point.x, point.y, point.z + epsilon)) - sdfDist(vec3(point.x, point.y, point.z - epsilon))
            ));
        }

        float shortestDistance(vec3 eye, vec3 dir) {
            float depth = minDist;
            float end = maxDist;
            for (int i = 0; i < marchingSteps; i++) {
                float dist = sdfDist(eye + depth * dir);
                if (dist < epsilon) {
                    return depth;
                }
                depth += dist;
                if (depth >= end) {
                    /* Moved beyond end */
                    return end;
                }
            }
            return end;
        }

        void main() {
            vec2 viewport = vec2(1.0, 1.0);
            vec2 fragCoord = |}
    ++ fragCoords
    ++ {|;
            vec3 eye = vec3(0.0, 0.0, 5.0);
            vec3 pixelEye = vec3(fragCoord, 4.0);
            float dist = shortestDistance(pixelEye, vec3(0.0, 0.0, -1.0));
            // All points should hit a shape in this shader
            vec3 p = pixelEye + dist * vec3(0.0, 0.0, -1.0);
            vec3 color = vec3(0.6, 0.6, 0.6);
            vec3 N = estimateNormal(p);
            // todo: Adjust estimateNormal?
            N.y = N.y * -1.0;
            float light = (lighting(p, vec3(vScreenPos.xy, p.z), N)).x;
            color = color * light;
            float alpha = |}
    ++ glAlpha
    ++ {|;
            gl_FragColor = vec4(|}
    ++ glColor
    ++ {|, alpha);
        }
    |};
  source;
};

let make =
    (
      sdfDist,
      fragCoords,
      model,
      lighting,
      ~width=1.0,
      ~height=1.0,
      ~color=NoColor,
      ~colorMix=0.4,
      ~vo=?,
      ~opacity=?,
      ~alphaLimit=?,
      ()
    ) => {
  sdfDist,
  fragCoords,
  width,
  height,
  model,
  color,
  colorMix,
  vo,
  opacity,
  alphaLimit,
  lighting
};

let makeProgram = self => {
  let requiredUniforms =
    switch self.model {
    | Some(_) => [("model", Gpu.GlType.Mat3f)]
    | None => []
    };
  let requiredUniforms =
    switch self.color {
    | SdfDynColor(_) => [("uColor", Gpu.GlType.Vec3f), ...requiredUniforms]
    | _ => requiredUniforms
    };
  Scene.makeProgram(
    ~vertShader=Shader.make(makeVertexSource(self)),
    ~fragShader=Shader.make(makeFragmentSource(self)),
    ~attribs=Gpu.VertexBuffer.quadAttribs(),
    ~requiredUniforms,
    ()
  );
};

/* Hash for sdfNode, will not take into account differences
   in sdfDist code */
type hash = {
  hFragCoords: fragCoords,
  hModel: bool,
  /* Simple encoding,
     None = no color,
     Some(None) = dyncolor,
     Some(Some(Color.t)) = Static */
  hColor: option(option(Color.t)),
  hColorMix: float,
  hAlphaLimit: option(float),
  hOpacity: option(float),
  hLight: Light.ProgramLight.hash
};

let makeHash = sdfNode => {
  let hModel =
    switch sdfNode.model {
    | Some(_) => true
    | None => false
    };
  let hColor =
    switch sdfNode.color {
    | SdfDynColor(_) => Some(None)
    | NoColor => None
    | SdfStaticColor(color) => Some(Some(color))
    };
  {
    hFragCoords: sdfNode.fragCoords,
    hModel,
    hColor,
    hColorMix: sdfNode.colorMix,
    hAlphaLimit: sdfNode.alphaLimit,
    hOpacity: sdfNode.opacity,
    hLight: Light.ProgramLight.makeHash(sdfNode.lighting)
  };
};

let makeNode =
    (
      self,
      ~key=?,
      ~cls="sdfNode",
      ~aspect=?,
      ~drawTo=?,
      ~margin=?,
      ~children=[],
      ~program=?,
      ()
    ) => {
  let transparent =
    switch (self.opacity, self.alphaLimit) {
    | (_, Some(_alphaLimit)) => true
    | (Some(opacity), _) => opacity < 1.0
    | _ => false
    };
  let uniforms =
    switch self.model {
    | Some(model) => [("model", model)]
    | None => []
    };
  let uniforms =
    switch self.color {
    | SdfDynColor(color) => [("uColor", color), ...uniforms]
    | _ => uniforms
    };
  let size =
    switch aspect {
    | Some(aspect) => Scene.Aspect(aspect)
    | None => Dimensions(Scale(self.width), Scale(self.height))
    };
  let partialDraw =
    switch self.vo {
    | Some(_) => true
    | None => false
    };
  let (vertShader, fragShader, program) =
    switch program {
    | Some(program) => (None, None, Some(program))
    | None => (
        Some(Shader.make(makeVertexSource(self))),
        Some(Shader.make(makeFragmentSource(self))),
        None
      )
    };
  Scene.makeNode(
    ~key?,
    ~cls,
    ~vertShader?,
    ~fragShader?,
    ~program?,
    ~size,
    ~margin?,
    ~transparent,
    ~uniforms,
    ~children,
    ~vo=?self.vo,
    ~partialDraw,
    ~clearOnDraw=partialDraw,
    ~drawTo?,
    ~texTransUniform=true,
    ()
  );
};
