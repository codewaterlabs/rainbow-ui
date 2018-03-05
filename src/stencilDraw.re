let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    void main() {
        gl_Position = vec4(position, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;

    void main() {
        gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
|};

open Gpu;

type t = {
  canvas: Canvas.t,
  program: Program.inited
};

let make = (canvas: Gpu.Canvas.t) => {
  let programT =
    Program.make(
      Shader.make(vertexSource),
      Shader.make(fragmentSource),
      VertexBuffer.quadAttribs(),
      []
    );
  let program =
    switch (Program.init(programT, canvas.context)) {
    | Some(program) => program
    | None => failwith("Could not initialize stencilDraw")
    };
  {canvas, program};
};

let draw = (self, vertexBuffer, indexBuffer) =>
  Gpu.Canvas.drawIndexes(
    self.canvas,
    self.program,
    vertexBuffer,
    indexBuffer,
    [||]
  );
