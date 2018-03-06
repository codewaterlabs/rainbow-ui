let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    void main() {
        gl_Position = vec4(position, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;
    uniform vec3 color;

    void main() {
        gl_FragColor = vec4(color, 1.0);
    }
|};

let makeNode = (
    ~color=?,
    ~uniform=?,
    ~hidden=?,
    ()
) => {
  let uniform = switch (color, uniform) {
  | (Some(color), None) => Scene.UVec3f.vec(Color.toVec3(color))
  | (Some(color), Some(uniform)) =>
    Scene.UVec3f.setQuiet(uniform, color);
    uniform
  | (None, Some(uniform)) => uniform
  | (None, None) => failwith("Color or uniform required")
  };
  Scene.(
    makeNode(
      ~cls="colorNode",
      ~vertShader=Gpu.Shader.make(vertexSource),
      ~fragShader=Gpu.Shader.make(fragmentSource),
      ~hidden=?hidden,
      ~uniforms=[("color", uniform)],
      ()
    )
  )
};