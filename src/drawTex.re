let vertShader = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    uniform mat3 texMat;
    varying vec2 texUV;
    void main() {
        texUV = (vec3(position, 1.0) * texMat).xy;
        vec2 pos = (vec3(position, 1.0) * layout).xy;
        gl_Position = vec4(pos, 0.0, 1.0);
    }
|};

let fragShader = {|
    precision mediump float;
    varying vec2 texUV;
    uniform sampler2D tex;

    void main() {
        vec4 texColor = texture2D(tex, texUV);
        gl_FragColor = vec4(smoothstep(vec2(0.0, 1.0), vec2(0.1, 0.9), texUV), 0.0, 1.0);
        gl_FragColor = texColor;
    }
|};

/* Should accept regular texture (and image) as well
   Similar to CacheResult */
let makeNode = (texNode, ~transparent=?, ~partialDraw=?, ~key=?, ~size=?, ()) =>
  Scene.makeNode(
    ~key?,
    ~cls="drawTex",
    ~vertShader=Gpu.Shader.make(vertShader),
    ~fragShader=Gpu.Shader.make(fragShader),
    ~transparent?,
    ~partialDraw?,
    ~size?,
    ~deps=[texNode],
    ~textures=[("tex", Scene.SceneTex.node(texNode))],
    ()
  );
