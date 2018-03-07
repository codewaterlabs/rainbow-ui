type sceneState = {placeholder: int};

let setup = _canvas : sceneState => {placeholder: 0};

let createRootNode = _state => {
    Scene.(Layout.vertical(
        ~size=Dimensions(Scale(1.0), Scale(1.0)),
        ~margin=Margin(Scale(0.0)),
        [
            ColorNode.makeNode(
                ~color=Color.make(0.5, 0.4, 0.6),
                ~size=WidthRatio(Scale(0.5), 2.0),
                ~maxHeight=Pixels(500.0),
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