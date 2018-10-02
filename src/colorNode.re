let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    void main() {
        gl_Position = vec4((vec3(position, 1.0) * layout).xy, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;
    uniform vec3 color;

    void main() {
        gl_FragColor = vec4(color, 1.0);
    }
|};

/* Todo: Consider better api/more general way to
   expose generic layout properties */
let makeNode = (
    ~color=?,
    ~uniform=?,
    ~hidden=?,
    ~size=?,
    ~margin=?,
    ~maxWidth=?,
    ~maxHeight=?,
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
      ~hidden?,
      ~size?,
      ~margin?,
      ~maxWidth?,
      ~maxHeight?,
      ~uniforms=[("color", uniform)],
      ()
    )
  )
};
