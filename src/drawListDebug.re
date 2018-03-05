let vertexSource = {|
    precision mediump float;
    attribute vec2 position;
    uniform mat3 layout;
    void main() {
        vec2 pos = (vec3(position, 1.0) * layout).xy;
        gl_Position = vec4(pos, 0.0, 1.0);
    }
|};

let fragmentSource = {|
    precision mediump float;
    uniform float elapsed;
    const float stepSize = 5.0;
    void main() {
        float stepMod = mod(elapsed, stepSize * 3.0);
        float r = smoothstep(0.0, stepSize, stepMod);
        float g = smoothstep(stepSize, stepSize * 2.0, stepMod);
        float b = smoothstep(stepSize * 2.0, stepSize * 3.0, stepMod);
        gl_FragColor = vec4(r, g, b, 0.1);
    }
|};

open Gpu;

type t = {
  canvas: Canvas.t,
  program: Program.inited,
  layout: Gpu.uniform,
  elapsed: Gpu.uniform,
  vertices: Gpu.VertexBuffer.inited,
  indices: Gpu.IndexBuffer.inited
};

let make = (canvas: Gpu.Canvas.t) => {
  let layout = Gpu.UniformMat3f(ref(Data.Mat3.id()));
  let elapsed = Gpu.UniformFloat(ref(0.0));
  let programT =
    Program.make(
      Shader.make(vertexSource),
      Shader.make(fragmentSource),
      VertexBuffer.quadAttribs(),
      [Uniform.make("layout", layout), Uniform.make("elapsed", elapsed)]
    );
  let program =
    switch (Program.init(programT, canvas.context)) {
    | Some(program) => program
    | None => failwith("Could not initialize stencilDraw")
    };
  let vertices =
    VertexBuffer.init(
      VertexBuffer.makeQuad(),
      canvas.context,
      canvas.gpuState
    );
  let indices =
    IndexBuffer.init(IndexBuffer.makeQuad(), canvas.context, canvas.gpuState);
  {canvas, program, layout, elapsed, vertices, indices};
};

let draw = (self, layoutMat, elapsed) => {
  Gpu.Uniform.setMat3f(self.layout, layoutMat);
  Gpu.Uniform.setFloat(self.elapsed, elapsed);
  Gpu.Canvas.drawIndexes(
    self.canvas,
    self.program,
    self.vertices,
    self.indices,
    [||]
  );
};
