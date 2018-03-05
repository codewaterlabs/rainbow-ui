let blurVertex = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    uniform mat3 origMat;
    varying vec2 vPosition;
    varying vec2 origPos;
    varying vec2 texMax;
    varying vec2 texMin;
    uniform vec2 pixelSize;
    varying vec2 pSize;
    void main() {
        vPosition = position;
        vec2 pos = (vec3(position, 1.0) * layout).xy;
        origPos = (vec3(position, 1.0) * origMat).xy;
        // Max right and bottom
        texMax = (vec3(1.0, -1.0, 1.0) * origMat).xy;
        // Min left and top
        texMin = (vec3(-1.0, 1.0, 1.0) * origMat).xy;
        float aspect = pixelSize.x / pixelSize.y;
        pSize = vec2(texMax.x / 300.0, texMin.y / 300.0 * aspect);
        gl_Position = vec4(pos, 0.0, 1.0);
    }
|};

let blurFragment = {|
    precision mediump float;
    varying vec2 vPosition;
    varying vec2 origPos;
    varying vec2 texMax;
    varying vec2 texMin;

    uniform float pDistance;
    uniform vec2 pixelSize;
    varying vec2 pSize;

    uniform sampler2D orig;

    float blurred(vec2 point, float pHeight, float pWidth) {
        // Something like circular box blur
        // should approximate bokeh effect
        // https://en.wikipedia.org/wiki/Gaussian_blur
        // [ 0   .8  1   .8  0  ]
        // [ .8  1   1   1   .8 ]
        // [ 1   1  -1-  1   1  ]
        // [ .8  1   1   1   .8 ]
        // [ 0   .8  1   .8  0  ]
        float maxLeft = texMin.x - point.x;
        float maxBottom = texMax.y - point.y;
        float maxRight = texMax.x - point.x;
        float maxTop = texMin.y - point.y;
        return (
            texture2D(orig, point + vec2(max(maxLeft, -pWidth), max(maxBottom, -pHeight*2.0))).x * 0.8 +
            texture2D(orig, point + vec2(0.0, max(maxBottom, -pHeight*2.0))).x +
            texture2D(orig, point + vec2(min(maxRight, pWidth), max(maxBottom, -pHeight*2.0))).x * 0.8 +

            texture2D(orig, point + vec2(max(maxLeft, -pWidth*2.0), max(maxBottom, -pHeight))).x * 0.8 +
            texture2D(orig, point + vec2(max(maxLeft, -pWidth), max(maxBottom, -pHeight))).x +
            texture2D(orig, point + vec2(0.0, max(maxBottom, -pHeight))).x +
            texture2D(orig, point + vec2(min(maxRight, pWidth), max(maxBottom, -pHeight))).x +
            texture2D(orig, point + vec2(min(maxRight, pWidth*2.0), max(maxBottom, -pHeight))).x * 0.8 +

            texture2D(orig, point + vec2(max(maxLeft, -pWidth*2.0), 0.0)).x +
            texture2D(orig, point + vec2(max(maxLeft, -pWidth), 0.0)).x +
            texture2D(orig, point + vec2(0.0, 0.0)).x +
            texture2D(orig, point + vec2(min(maxRight, pWidth), 0.0)).x +
            texture2D(orig, point + vec2(min(maxRight, pWidth*2.0), 0.0)).x +

            texture2D(orig, point + vec2(max(maxLeft, -pWidth*2.0), min(maxTop, pHeight))).x * 0.8 +
            texture2D(orig, point + vec2(max(maxLeft, -pWidth), min(maxTop, pHeight))).x +
            texture2D(orig, point + vec2(0.0, min(maxTop, pHeight))).x +
            texture2D(orig, point + vec2(min(maxRight, pWidth), min(maxTop, pHeight))).x +
            texture2D(orig, point + vec2(min(maxRight, pWidth*2.0), min(maxTop, pHeight))).x * 0.8 +

            texture2D(orig, point + vec2(max(maxLeft, -pWidth), min(maxTop, pHeight*2.0))).x * 0.8 +
            texture2D(orig, point + vec2(0.0, min(maxTop, pHeight*2.0))).x +
            texture2D(orig, point + vec2(min(maxRight, pWidth), min(maxTop, pHeight*2.0))).x * 0.8
        ) / (13.0 + 0.8 * 8.0);
    }

    void main() {
        float pHeight = pSize.y * pDistance;
        float pWidth = pSize.x * pDistance;
        float b = blurred(origPos, pHeight, pWidth);
        gl_FragColor = vec4(b, b, b, 1.0);

        /*
        float maxLeft = texMin.x - origPos.x;
        float maxBottom = texMax.y - origPos.y;
        float maxRight = texMax.x - origPos.x;
        float maxTop = texMin.y - origPos.y;
        gl_FragColor = vec4(maxLeft, maxBottom*-1., maxRight, 1.0);
        */
        //gl_FragColor = vec4(texture2D(orig, origPos).x, 1.0, 0.0, 1.0);
    }
|};

open Gpu;

let blurProgram = ref(None);

let makeNode = (origNode, toTex, tempTex, step1, step2) => {
  let blurProgram =
    switch blurProgram^ {
    | Some(program) => program
    | None =>
      let program =
        Scene.makeProgram(
          ~vertShader=Shader.make(blurVertex),
          ~fragShader=Shader.make(blurFragment),
          ~requiredUniforms=[("pDistance", GlType.Float)],
          ~requiredTextures=[("orig", true)],
          ~pixelSizeUniform=true,
          ~attribs=Gpu.VertexBuffer.quadAttribs(),
          ~vo=Scene.defaultVo(),
          ()
        );
      blurProgram := Some(program);
      program;
    };
  /* First blur */
  let blur1 =
    Scene.makeNode(
      ~cls="blur1",
      ~program=blurProgram,
      ~uniforms=[("pDistance", Scene.UFloat.make(step1))],
      ~textures=[("orig", Scene.SceneTex.node(origNode))],
      ~drawTo=Scene.TextureItem(tempTex),
      ~deps=[origNode],
      ()
    );
  /* Second blur */
  Scene.makeNode(
    ~cls="blur2",
    ~program=blurProgram,
    ~uniforms=[("pDistance", Scene.UFloat.make(step2))],
    ~textures=[("orig", Scene.SceneTex.node(blur1))],
    ~drawTo=Scene.TextureItem(toTex),
    ~deps=[blur1],
    ()
  );
};
