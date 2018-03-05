let vertexSource = {|
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

let fragmentSource = {|
    precision mediump float;
    varying vec2 texUV;
    uniform sampler2D tex;

    void main() {
        vec4 texColor = texture2D(tex, texUV);
        gl_FragColor = vec4(texColor);
    }
|};

open Gpu;

let makeNode = (~node, ()) =>
  Scene.makeNode(
    ~cls="textureCopy",
    ~vertShader=Shader.make(vertexSource),
    ~fragShader=Shader.make(fragmentSource),
    ~textures=[("tex", Scene.SceneTex.node(node))],
    ~drawTo=Scene.TextureRGB,
    ()
  );
