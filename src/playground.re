type sceneState = {placeholder: int};

let setup = _canvas : sceneState => {placeholder: 0};

let vertex1 = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    varying vec2 posColor;
    void main() {
        posColor = position * 0.2;
        gl_Position = vec4((vec3(position, 1.0) * layout).xy, 0.0, 1.0);
    }
|};

let fragment1 = {|
    precision mediump float;
    varying vec2 posColor;

    void main() {
        gl_FragColor = vec4(posColor, 0.0, 1.0);
    }
|};

let vertex2 = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    varying vec2 posColor;
    uniform mat3 other;
    void main() {
        posColor = (vec3(position, 1.0) * other).xy * 0.2;
        gl_Position = vec4((vec3(position, 1.0) * layout).xy, 0.0, 1.0);
    }
|};

let fragment2 = {|
    precision mediump float;
    varying vec2 posColor;

    void main() {
        gl_FragColor = vec4(posColor, 0.0, 1.0);
    }
|};
let createRootNode = _state => {
    Scene.(Layout.horizontal(
        ~size=Dimensions(Scale(1.0), Scale(1.0)),
        ~margin=Margin(Scale(0.0)),
        ~hAlign=Scene.AlignCenter,
        ~spacing=Scene.Scale(0.01),
        [
            Scene.makeNode(
                ~key="test1",
                ~vertShader=Gpu.Shader.make(vertex1),
                ~fragShader=Gpu.Shader.make(fragment1),
                ~size=WidthRatio(Scale(0.3), 2.0),
                ~maxHeight=Pixels(500.0),
                ()
            ),
            Scene.makeNode(
                ~vertShader=Gpu.Shader.make(vertex2),
                ~fragShader=Gpu.Shader.make(fragment2),
                ~size=WidthRatio(Scale(0.3), 2.0),
                ~maxHeight=Pixels(500.0),
                ~nodeScaleUniforms=[("test1", "other")],
                ()
            )
        ]
    ))
};

let createScene = (canvas, state) =>
  Scene.make(canvas, state, createRootNode(state), ~drawListDebug=false, ());

let draw = (state, scene, _canvas: Gpu.Canvas.t) => {
  Scene.update(scene);
  state;
};

let keyPressed = (state, _canvas) => state;

let resize = _state => ();

let (viewportX, viewportY) = Gpu.Canvas.getViewportSize();

Scene.run(
  viewportX,
  viewportY,
  setup,
  createScene,
  draw,
  ~keyPressed,
  ~resize,
  ()
);