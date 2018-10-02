module Constants = RGLConstants;

module Gl = Reasongl.Gl;

module GlType = {
  type t =
    | Float
    | Int
    | Vec2f
    | Vec3f
    | Vec4f
    | Mat3f
    | Mat4f;
  let getSize = glType =>
    switch glType {
    | Float
    | Int => 1
    | Vec2f => 2
    | Vec3f => 3
    | Vec4f => 4
    | Mat3f => 9
    | Mat4f => 16
    };
  let getBytes = glType =>
    switch glType {
    | Float
    | Vec2f
    | Vec3f
    | Vec4f
    | Mat3f
    | Mat4f => 4
    | Int => 2
    };
  let getTypeConst = glType =>
    switch glType {
    | Float
    | Vec2f
    | Vec3f
    | Vec4f
    | Mat3f
    | Mat4f => Constants.float_
    | Int => Constants.unsigned_short
    };
};

type bufferUsage =
  | StaticDraw
  | DynamicDraw
  | StreamingDraw;

open Data;

/* Put here to avoid curry since currently bucklescript
   does not uncurry functions with labelled arguments */
[@bs.send] external glEnable : (Gl.contextT, int) => unit = "enable";

[@bs.send] external glDisable : (Gl.contextT, int) => unit = "disable";

[@bs.send]
external glBlendFunc : (Gl.contextT, int, int) => unit = "blendFunc";

/* Context, target, buffer */
[@bs.send]
external glBindBuffer : (Gl.contextT, int, Gl.bufferT) => unit = "bindBuffer";

/* Resource id, these are used for programs,
   vertex and index buffers, textures */
let curRId = ref(1);

/* Not thread safe :) */
let rId = () => {
  let rId = curRId^;
  curRId := rId + 1;
  rId;
};

type gpuState = {
  mutable curProg: int,
  mutable curVertex: int,
  mutable curIndex: int,
  mutable curTextures: array(int)
};

type uniform =
  | UniformFloat(ref(float))
  | UniformInt(ref(int))
  | UniformVec2f(ref(Vec2.t))
  | UniformVec3f(ref(Vec3.t))
  | UniformVec4f(ref(Vec4.t))
  | UniformMat3f(ref(Mat3.t))
  | UniformMat4f(ref(Mat4.t));

module Uniform = {
  type inited = {
    glType: GlType.t,
    loc: Gl.uniformT,
    uniform
  };
  type t = {
    name: string,
    glType: GlType.t,
    uniform,
    mutable inited: option(inited)
  };
  let getUniformGlType = uniform =>
    switch uniform {
    | UniformFloat(_) => GlType.Float
    | UniformInt(_) => GlType.Int
    | UniformVec2f(_) => GlType.Vec2f
    | UniformVec3f(_) => GlType.Vec3f
    | UniformVec4f(_) => GlType.Vec4f
    | UniformMat3f(_) => GlType.Mat3f
    | UniformMat4f(_) => GlType.Mat4f
    };
  /* Todo: Maybe a different name for this type
     to avoid confusion with Uniform.t? Like
     uniformValue(Holder) */
  let makeValRefFromType = glType =>
    switch glType {
    | GlType.Float => UniformFloat(ref(0.0))
    | GlType.Vec2f => UniformVec2f(ref(Data.Vec2.zeros()))
    | GlType.Vec3f => UniformVec3f(ref(Data.Vec3.zeros()))
    | GlType.Vec4f => UniformVec4f(ref(Data.Vec4.zeros()))
    | GlType.Mat3f => UniformMat3f(ref(Data.Mat3.id()))
    | GlType.Mat4f => UniformMat4f(ref(Data.Mat4.id()))
    | GlType.Int => UniformInt(ref(0))
    };
  let make = (name, uniform) => {
    name,
    glType: getUniformGlType(uniform),
    uniform,
    inited: None
  };
  let init = (uniform, context, program) => {
    let inited = {
      glType: uniform.glType,
      uniform: uniform.uniform,
      loc: Gl.getUniformLocation(~context, ~program, ~name=uniform.name)
    };
    uniform.inited = Some(inited);
    inited;
  };
  let initLoc = (context, program, name) =>
    Gl.getUniformLocation(~context, ~program, ~name);
  let initedFromLoc = (glType, uniform, loc) : inited => {
    glType,
    uniform,
    loc
  };
  let setFloat = (uniform, value) =>
    switch uniform {
    | UniformFloat(valRef) => valRef := value
    | _ => failwith("Uniform not float")
    };
  let setVec2f = (uniform, value) =>
    switch uniform {
    | UniformVec2f(valRef) => valRef := value
    | _ => failwith("Uniform not vec2f")
    };
  let setVec3f = (uniform, value) =>
    switch uniform {
    | UniformVec3f(valRef) => valRef := value
    | _ => failwith("Uniform not vec3f")
    };
  let setVec4f = (uniform, value) =>
    switch uniform {
    | UniformVec4f(valRef) => valRef := value
    | _ => failwith("Uniform not vec4f")
    };
  let setMat3f = (uniform, value) =>
    switch uniform {
    | UniformMat3f(valRef) => valRef := value
    | _ => failwith("Uniform not mat3")
    };
  [@bs.send]
  external uniformMatrix3fv :
    (Gl.contextT, Gl.uniformT, bool, array(float)) => unit =
    "uniformMatrix3fv";
  [@bs.send]
  external uniformMatrix4fv :
    (Gl.contextT, Gl.uniformT, bool, array(float)) => unit =
    "uniformMatrix4fv";
  /* Put here to avoid currying while bucklescript doesnt yet optimize labelled arguments */
  [@bs.send]
  external uniform1i : (Gl.contextT, Gl.uniformT, int) => unit = "uniform1i";
  [@bs.send]
  external uniform1f : (Gl.contextT, Gl.uniformT, float) => unit = "uniform1f";
  [@bs.send]
  external uniform2f : (Gl.contextT, Gl.uniformT, float, float) => unit =
    "uniform2f";
  [@bs.send]
  external uniform3f : (Gl.contextT, Gl.uniformT, float, float, float) => unit =
    "uniform3f";
  [@bs.send]
  external uniform4f :
    (Gl.contextT, Gl.uniformT, float, float, float, float) => unit =
    "uniform4f";
  let valueToGpu = (uniform: inited, context) =>
    switch uniform.uniform {
    | UniformFloat(value) => uniform1f(context, uniform.loc, value^)
    | UniformInt(value) => uniform1i(context, uniform.loc, value^)
    | UniformVec2f(value) =>
      let value = value^;
      uniform2f(context, uniform.loc, value[0], value[1]);
    | UniformVec3f(value) =>
      let value = value^;
      uniform3f(context, uniform.loc, value[0], value[1], value[2]);
    | UniformVec4f(value) =>
      let value = value^;
      uniform4f(context, uniform.loc, value[0], value[1], value[2], value[3]);
    | UniformMat3f(values) =>
      let values = values^;
      uniformMatrix3fv(context, uniform.loc, false, values);
    | UniformMat4f(values) =>
      let values = values^;
      uniformMatrix4fv(context, uniform.loc, false, values);
    };
};

module VertexAttrib = {
  type t = {
    name: string,
    glType: GlType.t
  };
  type inited = {
    loc: Gl.attributeT,
    size: int,
    stride: int,
    offset: int,
    type_: int
  };
  let make = (name, glType) => {name, glType};
  let init = (attrib, context, program, size, stride, offset) => {
    let loc = Gl.getAttribLocation(~context, ~program, ~name=attrib.name);
    {loc, size, stride, offset, type_: GlType.getTypeConst(attrib.glType)};
  };
  /* context, attribute, size, type, normalize, stride, offset */
  [@bs.send]
  external _vertexAttribPointer :
    (Gl.contextT, Gl.attributeT, int, int, bool, int, int) => unit =
    "vertexAttribPointer";
  let setPointer = (inited, context) =>
    _vertexAttribPointer(
      context,
      inited.loc,
      inited.size,
      inited.type_,
      false,
      inited.stride,
      inited.offset
    );
};

module Shader = {
  type t = {source: string};
  let make = source => {source: source};
  let compileShader = (context, shaderType, source) => {
    let shaderHandle = Gl.createShader(~context, shaderType);
    Gl.shaderSource(~context, ~shader=shaderHandle, ~source);
    Gl.compileShader(~context, shaderHandle);
    let compiledCorrectly =
      Gl.getShaderParameter(
        ~context,
        ~shader=shaderHandle,
        ~paramName=Gl.Compile_status
      )
      == 1;
    if (! compiledCorrectly) {
      print_endline(
        "Shader compilation failed: "
        ++ Gl.getShaderInfoLog(~context, shaderHandle)
      );
      print_endline(source);
      None;
    } else {
      Some(shaderHandle);
    };
  };
};

module Program = {
  type t = {
    vertexShader: Shader.t,
    fragmentShader: Shader.t,
    attribs: list(VertexAttrib.t),
    uniforms: list(Uniform.t),
    perElement: int
  };
  type inited = {
    rId: int,
    programRef: Gl.programT,
    attribs: list(VertexAttrib.inited),
    uniforms: list(Uniform.inited)
  };
  let make = (vertexShader, fragmentShader, attribs, uniforms) => {
    let perElement =
      List.fold_left(
        (p, attrib: VertexAttrib.t) => p + GlType.getSize(attrib.glType),
        0,
        attribs
      );
    {vertexShader, fragmentShader, attribs, uniforms, perElement};
  };
  let linkProgram = (context, vertexSource, fragmentSource) => {
    let vShader =
      Shader.compileShader(context, Constants.vertex_shader, vertexSource);
    let fShader =
      Shader.compileShader(context, Constants.fragment_shader, fragmentSource);
    switch (vShader, fShader) {
    | (Some(vShader), Some(fShader)) =>
      let program = Gl.createProgram(~context);
      Gl.attachShader(~context, ~program, ~shader=vShader);
      Gl.deleteShader(~context, vShader);
      Gl.attachShader(~context, ~program, ~shader=fShader);
      Gl.deleteShader(~context, fShader);
      Gl.linkProgram(~context, program);
      let linkedCorrectly =
        Gl.getProgramParameter(~context, ~program, ~paramName=Gl.Link_status)
        == 1;
      if (linkedCorrectly) {
        Some(program);
      } else {
        print_endline("GlProgram linking failed");
        None;
      };
    | _ => None
    };
  };
  let initAttribs = (attribs, context, program) => {
    let stride =
      List.fold_left(
        (size, attrib: VertexAttrib.t) =>
          size + GlType.getSize(attrib.glType) * GlType.getBytes(attrib.glType),
        0,
        attribs
      );
    let offset = ref(0);
    List.map(
      (attrib: VertexAttrib.t) => {
        let size = GlType.getSize(attrib.glType);
        let attrSize = size * GlType.getBytes(attrib.glType);
        let loc =
          VertexAttrib.init(attrib, context, program, size, stride, offset^);
        offset := offset^ + attrSize;
        loc;
      },
      attribs
    );
  };
  let init = (program, context) =>
    switch (
      linkProgram(
        context,
        program.vertexShader.source,
        program.fragmentShader.source
      )
    ) {
    | Some(programRef) =>
      let uniforms =
        List.map(
          uniform => Uniform.init(uniform, context, programRef),
          program.uniforms
        );
      let attribs = initAttribs(program.attribs, context, programRef);
      Some({rId: rId(), programRef, uniforms, attribs});
    | None => None
    };
  let initedWithUniforms = (iProgram: inited, uniforms) => {
    rId: iProgram.rId,
    programRef: iProgram.programRef,
    uniforms,
    attribs: iProgram.attribs
  };
};

[@bs.send]
external _deleteBuffer : (Gl.contextT, Gl.bufferT) => unit = "deleteBuffer";

/* context, target, data, usage */
[@bs.send]
external _bufferData : (Gl.contextT, int, Gl.Bigarray.t('a, 'b), int) => unit =
  "bufferData";

module VertexBuffer = {
  type inited = {
    rId: int,
    bufferRef: Gl.bufferT,
    mutable data: array(float),
    perElement: int,
    mutable update: bool,
    mutable count: int,
    usage: bufferUsage
  };
  type t = {
    mutable data: array(float),
    usage: bufferUsage,
    attribs: list((string, GlType.t)),
    perElement: int,
    mutable inited: option(inited)
  };
  let make = (data, attribs, usage) => {
    let perElement =
      List.fold_left(
        (p, (_, attrib: GlType.t)) => p + GlType.getSize(attrib),
        0,
        attribs
      );
    {data, usage, attribs, perElement, inited: None};
  };
  let createAttribs = self =>
    List.map(
      ((name, glType)) => VertexAttrib.make(name, glType),
      self.attribs
    );
  /* Width height in 0.0 - 2.0, x, y from top left as 0.0 for now */
  let makeQuadData = (width, height, x, y) => {
    let leftX = (-1.) +. x;
    let rightX = leftX +. width;
    let topY = 1.0 -. y;
    let bottomY = topY -. height;
    [|leftX, bottomY, rightX, bottomY, rightX, topY, leftX, topY|];
  };
  let quadAttribs = () => [VertexAttrib.make("position", Vec2f)];
  let makeQuad = (~data=?, ~usage=StaticDraw, ()) => {
    let data =
      switch data {
      | Some(data) => data
      | None => makeQuadData(2.0, 2.0, 0.0, 0.0)
      };
    {
      data,
      usage,
      attribs: [("position", GlType.Vec2f)],
      perElement: 2,
      inited: None
    };
  };
  let setData = (inited: inited, data) => {
    inited.data = data;
    inited.count = Array.length(data);
    inited.update = true;
  };
  let setDataT = (self: t, data) =>
    switch self.inited {
    | Some(inited) => setData(inited, data)
    | None => self.data = data
    };
  let updateGpuData = (inited: inited, context) => {
    inited.count = Array.length(inited.data);
    _bufferData(
      context,
      Constants.array_buffer,
      Gl.Bigarray.of_array(Gl.Bigarray.Float32, inited.data),
      switch inited.usage {
      | StaticDraw => Constants.static_draw
      | DynamicDraw => Constants.dynamic_draw
      | StreamingDraw => Constants.stream_draw
      }
    );
  };
  let init = (buffer, context, gpuState) =>
    switch buffer.inited {
    | Some(inited) => inited
    | None =>
      let vertexBuffer = Gl.createBuffer(~context);
      glBindBuffer(context, Constants.array_buffer, vertexBuffer);
      _bufferData(
        context,
        Constants.array_buffer,
        Gl.Bigarray.of_array(Gl.Bigarray.Float32, buffer.data),
        switch buffer.usage {
        | StaticDraw => Constants.static_draw
        | DynamicDraw => Constants.dynamic_draw
        | StreamingDraw => Constants.stream_draw
        }
      );
      let inited = {
        rId: rId(),
        bufferRef: vertexBuffer,
        data: buffer.data,
        update: false,
        perElement: buffer.perElement,
        count: Array.length(buffer.data),
        usage: buffer.usage
      };
      gpuState.curVertex = inited.rId;
      buffer.inited = Some(inited);
      inited;
    };
  let deleteBuffer = (context, buf) => _deleteBuffer(context, buf.bufferRef);
};

module IndexBuffer = {
  type inited = {
    rId: int,
    elBufferRef: Gl.bufferT,
    mutable data: array(int),
    mutable update: bool,
    mutable count: int,
    usage: bufferUsage,
    context: Gl.contextT
  };
  type t = {
    mutable data: array(int),
    usage: bufferUsage,
    mutable inited: option(inited)
  };
  let make = (data, usage) => {data, usage, inited: None};
  let makeQuad = () => {
    data: [|0, 1, 2, 0, 2, 3|],
    usage: StaticDraw,
    inited: None
  };
  let makeQuadsData = num => {
    let d = Array.make(num * 6, 0);
    let rec quadData = quadNum =>
      if (quadNum < num) {
        let offset = 4 * quadNum;
        let i = quadNum * 6;
        d[i] = 0 + offset;
        d[i + 1] = 1 + offset;
        d[i + 2] = 2 + offset;
        d[i + 3] = 0 + offset;
        d[i + 4] = 2 + offset;
        d[i + 5] = 3 + offset;
        quadData(quadNum + 1);
      };
    quadData(0);
    d;
  };
  let setData = (inited: inited, data) => {
    inited.data = data;
    inited.count = Array.length(data);
    inited.update = true;
  };
  let setDataT = (self: t, data) =>
    switch self.inited {
    | Some(inited) => setData(inited, data)
    | None => self.data = data
    };
  let updateGpuData = (inited: inited) => {
    let context = inited.context;
    inited.count = Array.length(inited.data);
    _bufferData(
      context,
      Constants.element_array_buffer,
      Gl.Bigarray.of_array(Gl.Bigarray.Uint16, inited.data),
      switch inited.usage {
      | StaticDraw => Constants.static_draw
      | DynamicDraw => Constants.dynamic_draw
      | StreamingDraw => Constants.stream_draw
      }
    );
  };
  let init = (buffer, context, gpuState) =>
    switch buffer.inited {
    | Some(inited) => inited
    | None =>
      let bufferRef = Gl.createBuffer(~context);
      glBindBuffer(context, Constants.element_array_buffer, bufferRef);
      _bufferData(
        context,
        Constants.element_array_buffer,
        Gl.Bigarray.of_array(Gl.Bigarray.Uint16, buffer.data),
        switch buffer.usage {
        | StaticDraw => Constants.static_draw
        | DynamicDraw => Constants.dynamic_draw
        | StreamingDraw => Constants.stream_draw
        }
      );
      let inited = {
        rId: rId(),
        elBufferRef: bufferRef,
        data: buffer.data,
        update: false,
        count: Array.length(buffer.data),
        usage: buffer.usage,
        context
      };
      gpuState.curIndex = inited.rId;
      buffer.inited = Some(inited);
      inited;
    };
  let deleteBuffer = (context, buf) => _deleteBuffer(context, buf.elBufferRef);
};

module VertexObject = {
  type t = {
    vertices: VertexBuffer.t,
    indices: option(IndexBuffer.t)
  };
  let make = (vertices, indices) => {vertices, indices};
  let makeQuad = (~usage=StaticDraw, ()) =>
    make(
      VertexBuffer.makeQuad(~usage, ()),
      Some(IndexBuffer.make(IndexBuffer.makeQuadsData(1), usage))
    );
  let updateQuads = (self, vertices) => {
    VertexBuffer.setDataT(self.vertices, vertices);
    let perElement = self.vertices.perElement * 4;
    switch self.indices {
    | Some(indices) =>
      IndexBuffer.setDataT(
        indices,
        IndexBuffer.makeQuadsData(Array.length(vertices) / perElement)
      )
    | None => ()
    };
  };
};

module Texture = {
  type format =
    | Luminance
    | Alpha
    | RGB
    | RGBA;
  type data =
    | ImageTexture(Reasongl.Gl.imageT)
    | IntDataTexture(option(array(int)), int, int)
    | EmptyTexture;
  type filter =
    | LinearFilter
    | NearestFilter;
  type inited = {
    rId: int,
    texRef: Gl.textureT,
    mutable data,
    mutable update: bool,
    format: int,
    context: Gl.contextT
  };
  type t = {
    mutable data,
    format,
    filter,
    mutable inited: option(inited)
  };
  let make = (data, format, filter) => {data, format, inited: None, filter};
  let setDataT = (self, data) =>
    switch self.inited {
    | Some(inited) =>
      inited.data = data;
      inited.update = true;
    | None => self.data = data
    };
  let getSize = texture => {
    let data =
      switch texture.inited {
      | Some(inited) => inited.data
      | None => texture.data
      };
    switch data {
    | IntDataTexture(_, width, height) => Some((width, height))
    | _ => None
    };
  };
  let luminance = 6409;
  let alpha = 6406;
  [@bs.send]
  external _texImage2D :
    (
      ~context: Gl.contextT,
      ~target: int,
      ~level: int,
      ~internalFormat: int,
      ~width: int,
      ~height: int,
      ~border: int,
      ~format: int,
      ~type_: int,
      ~data: Js.nullable(Gl.Bigarray.t('a, 'b))
    ) =>
    unit =
    "texImage2D";
  let unpackAlignment = 3317;
  [@bs.send]
  external _pixelStorei : (~context: Gl.contextT, int, int) => unit =
    "pixelStorei";
  let makeEmptyRgba = (~width=128, ~height=128, ~filter=LinearFilter, ()) =>
    make(IntDataTexture(None, width, height), RGBA, filter);
  let makeEmptyRgb = (~width=128, ~height=128, ~filter=LinearFilter, ()) =>
    make(IntDataTexture(None, width, height), RGB, filter);
  let makeEmptyGreyscale = (~width=128, ~height=128, ~filter=LinearFilter, ()) =>
    make(IntDataTexture(None, width, height), Alpha, filter);
  let init = (texture: t, context) =>
    switch texture.inited {
    | Some(inited) => inited
    | None =>
      let texRef = Gl.createTexture(~context);
      let filterConst =
        switch texture.filter {
        | LinearFilter => Constants.linear
        | NearestFilter => Constants.nearest
        };
      /* Not setting this to gpuState, assuming this is
         only needed together with activeTexture */
      Gl.bindTexture(~context, ~target=Constants.texture_2d, ~texture=texRef);
      Gl.texParameteri(
        ~context,
        ~target=Constants.texture_2d,
        ~pname=Constants.texture_wrap_s,
        ~param=Constants.clamp_to_edge
      );
      Gl.texParameteri(
        ~context,
        ~target=Constants.texture_2d,
        ~pname=Constants.texture_wrap_t,
        ~param=Constants.clamp_to_edge
      );
      Gl.texParameteri(
        ~context,
        ~target=Constants.texture_2d,
        ~pname=Constants.texture_min_filter,
        ~param=filterConst
      );
      Gl.texParameteri(
        ~context,
        ~target=Constants.texture_2d,
        ~pname=Constants.texture_mag_filter,
        ~param=filterConst
      );
      let format =
        switch texture.format {
        | RGBA => Constants.rgba
        | RGB => Constants.rgb
        | Alpha => alpha
        | Luminance => luminance
        };
      /* Luminance format gives 1 value per pixel repeated for rgba */
      switch texture.format {
      | Luminance => _pixelStorei(~context, unpackAlignment, 1)
      | _ => ()
      };
      switch texture.data {
      | ImageTexture(image) =>
        Reasongl.Gl.texImage2DWithImage(
          ~context,
          ~target=Constants.texture_2d,
          ~level=0,
          ~image
        )
      | IntDataTexture(intArray, width, height) =>
        _texImage2D(
          ~context,
          ~target=Constants.texture_2d,
          ~level=0,
          ~internalFormat=format,
          ~width,
          ~height,
          ~border=0,
          ~format,
          ~type_=RGLConstants.unsigned_byte,
          ~data=
            switch intArray {
            | Some(intArray) =>
              Js.Nullable.return(
                Gl.Bigarray.of_array(Gl.Bigarray.Uint8, intArray)
              )
            | None => Js.Nullable.null
            }
        )
      | EmptyTexture => ()
      };
      let inited = {
        rId: rId(),
        texRef,
        data: texture.data,
        update: false,
        format,
        context
      };
      texture.inited = Some(inited);
      inited;
    };
  let initOrBind = (texture: t, context) =>
    switch texture.inited {
    | Some(inited) =>
      Gl.bindTexture(
        ~context,
        ~target=Constants.texture_2d,
        ~texture=inited.texRef
      );
      inited;
    | None =>
      /* Will also bind */
      init(texture, context)
    };
  let ensureSize = (self: t, context, width, height) =>
    switch self.data {
    | IntDataTexture(data, w, h) =>
      if (w < width || h < height) {
        /* As I understand it, opengl want
           texture sizes in power of twos */
        let maxDim = max(width, height);
        let rec findDim = test =>
          if (maxDim <= test) {
            test;
          } else {
            findDim(test * 2);
          };
        let dim = findDim(1);
        let inited = initOrBind(self, context);
        let resizeArr = (arr, newSize) => {
          let prevLen = Array.length(arr);
          Array.init(newSize, i =>
            if (i < prevLen) {
              arr[i];
            } else {
              0;
            }
          );
        };
        let data =
          switch data {
          | None =>
            inited.data = IntDataTexture(None, dim, dim);
            self.data = inited.data;
            Js.Nullable.null;
          | Some(intArray) =>
            switch self.format {
            | RGB =>
              let data = resizeArr(intArray, dim * dim * 3);
              inited.data = IntDataTexture(Some(data), dim, dim);
              self.data = inited.data;
              Js.Nullable.return(
                Gl.Bigarray.of_array(Gl.Bigarray.Uint8, data)
              );
            | RGBA =>
              let data = resizeArr(intArray, dim * dim * 4);
              inited.data = IntDataTexture(Some(data), dim, dim);
              self.data = inited.data;
              Js.Nullable.return(
                Gl.Bigarray.of_array(Gl.Bigarray.Uint8, data)
              );
            | Luminance =>
              let data = resizeArr(intArray, dim * dim);
              inited.data = IntDataTexture(Some(data), dim, dim);
              self.data = inited.data;
              Js.Nullable.return(
                Gl.Bigarray.of_array(Gl.Bigarray.Uint8, data)
              );
            | Alpha =>
              let data = resizeArr(intArray, dim * dim);
              inited.data = IntDataTexture(Some(data), dim, dim);
              self.data = inited.data;
              Js.Nullable.return(
                Gl.Bigarray.of_array(Gl.Bigarray.Uint8, data)
              );
            }
          };
        _texImage2D(
          ~context,
          ~target=Constants.texture_2d,
          ~level=0,
          ~internalFormat=inited.format,
          ~width=dim,
          ~height=dim,
          ~border=0,
          ~format=inited.format,
          ~type_=RGLConstants.unsigned_byte,
          ~data
        );
      }
    | _ => failwith("Need int data texture to resize")
    };
  let updateGpuData = (inited: inited) => {
    let context = inited.context;
    Gl.bindTexture(
      ~context,
      ~target=Constants.texture_2d,
      ~texture=inited.texRef
    );
    switch inited.data {
    | ImageTexture(image) =>
      Reasongl.Gl.texImage2DWithImage(
        ~context,
        ~target=Constants.texture_2d,
        ~level=0,
        ~image
      )
    | IntDataTexture(data, width, height) =>
      _texImage2D(
        ~context,
        ~target=Constants.texture_2d,
        ~level=0,
        ~internalFormat=inited.format,
        ~width,
        ~height,
        ~border=0,
        ~format=inited.format,
        ~type_=RGLConstants.unsigned_byte,
        ~data=
          switch data {
          | Some(intArray) =>
            Js.Nullable.return(
              Gl.Bigarray.of_array(Gl.Bigarray.Uint8, intArray)
            )
          | None => Js.Nullable.null
          }
      )
    | EmptyTexture => ()
    };
    inited.update = false;
  };
};

module ProgramTexture = {
  type inited = {
    rId: int,
    texture: Texture.inited,
    uniformRef: Gl.uniformT
  };
  type t = {
    uniformName: string,
    texture: Texture.t,
    mutable inited: option(inited)
  };
  let make = (uniformName, texture) => {uniformName, texture, inited: None};
  let init = (programTexture, context, program) =>
    switch programTexture.inited {
    | Some(inited) => inited
    | None =>
      let tInit = Texture.init(programTexture.texture, context);
      let uRef =
        Gl.getUniformLocation(
          ~context,
          ~program,
          ~name=programTexture.uniformName
        );
      let inited = {rId: rId(), texture: tInit, uniformRef: uRef};
      programTexture.inited = Some(inited);
      inited;
    };
  /* Just a call to getUniformLocation. This way can
     be used to get a uniform location, then connect
     it with a texture at a later time (as in sceneProgram) */
  let initRef = (uniformName, context, program) =>
    Gl.getUniformLocation(~context, ~program, ~name=uniformName);
  let initFromRef = (programTexture, context, uRef) =>
    switch programTexture.inited {
    | Some(_) => failwith("Init from ref already inited")
    | None =>
      let tInit = Texture.init(programTexture.texture, context);
      let inited = {rId: rId(), texture: tInit, uniformRef: uRef};
      programTexture.inited = Some(inited);
      inited;
    };
};

module FrameBuffer = {
  type frameBufferT;
  let frameBufferC = 36160;
  let color_attachment0 = 36064;
  [@bs.send]
  external _createFramebuffer : (~context: Gl.contextT) => frameBufferT =
    "createFramebuffer";
  [@bs.send]
  external _bindFramebuffer :
    (~context: Gl.contextT, int, Js.Nullable.t(frameBufferT)) => unit =
    "bindFramebuffer";
  [@bs.send]
  external _framebufferTexture2d :
    (~context: Gl.contextT, int, int, int, Gl.textureT, int) => unit =
    "framebufferTexture2D";
  type inited = {
    width: int,
    height: int,
    frameBufferRef: frameBufferT
  };
  type t = {
    width: int,
    height: int,
    mutable inited: option(inited)
  };
  let make = (width, height) => {width, height, inited: None};
  let init = (frameBuffer: t, context) =>
    switch frameBuffer.inited {
    | Some(inited) => inited
    | None =>
      /* Setup framebuffer */
      let frameBufferRef = _createFramebuffer(~context);
      let inited = {
        width: frameBuffer.width,
        height: frameBuffer.height,
        frameBufferRef
      };
      frameBuffer.inited = Some(inited);
      inited;
    };
  let bindTexture = (frameBuffer: inited, context, texture) => {
    let texInit = Texture.init(texture, context);
    _bindFramebuffer(
      ~context,
      frameBufferC,
      Js.Nullable.return(frameBuffer.frameBufferRef)
    );
    _framebufferTexture2d(
      ~context,
      frameBufferC,
      color_attachment0,
      Constants.texture_2d,
      texInit.texRef,
      0
    );
  };
};

module Scissor = {
  let scissorTest = 3089;
  /* x, y, width, height
     all needs to be integers */
  [@bs.send]
  external scissor : (Gl.contextT, int, int, int, int) => unit = "scissor";
};

module Stencil = {
  let stencilTest = 2960;
  /* Stencil funcs */
  let always = 519;
  let equal = 514;
  let notEqual = 517;
  /* Stencil ops */
  let keep = 7680;
  let replace = 7681;
  /* context, func, ref, mask */
  [@bs.send]
  external stencilFunc : (Gl.contextT, int, int, int) => unit = "stencilFunc";
  /* context, fail, zfail, zpass */
  [@bs.send]
  external stencilOp : (Gl.contextT, int, int, int) => unit = "stencilOp";
  [@bs.send] external stencilMask : (Gl.contextT, int) => unit = "stencilMask";
};

module Error = {
  type glError =
    | NoError
    | InvalidEnum
    | InvalidValue
    | InvalidOperation
    | InvalidFramebufferOperation
    | OutOfMemory
    | ContextLostWebgl
    | OtherError(int);
  [@bs.send] external _getError : Gl.contextT => int = "getError";
  let getError = context =>
    switch (_getError(context)) {
    | 0 => NoError
    | 1280 => InvalidEnum
    | 1281 => InvalidValue
    | 1282 => InvalidOperation
    | 1286 => InvalidFramebufferOperation
    | 1285 => OutOfMemory
    | 37442 => ContextLostWebgl
    | other => OtherError(other)
    };
  let getErrorString = context =>
    switch (getError(context)) {
    | NoError => None
    | InvalidEnum => Some("Invalid enum")
    | InvalidValue => Some("Invalid value")
    | InvalidOperation => Some("Invalid operation")
    | InvalidFramebufferOperation => Some("Invalid framebuffer operation")
    | OutOfMemory => Some("Out of memory")
    | ContextLostWebgl => Some("Context lost webgl")
    | OtherError(other) => Some("Other error: " ++ string_of_int(other))
    };
};

module Canvas = {
  type keyboardT = {mutable keyCode: Reasongl.Gl.Events.keycodeT};
  type t = {
    context: Gl.contextT,
    gpuState,
    mutable deltaTime: float,
    mutable elapsed: float,
    mutable width: int,
    mutable height: int,
    keyboard: keyboardT,
    window: Gl.Window.t
  };
  type window;
  let window: window = [%bs.raw "window"];
  [@bs.get] external winInnerWidth : window => float = "innerWidth";
  [@bs.get] external winInnerHeight : window => float = "innerHeight";
  let getViewportSize = () => (
    int_of_float(winInnerWidth(window)),
    int_of_float(winInnerHeight(window))
  );
  /* Todo: Pull request to reasongl to enable parameters? */
  external __destructureWindowHack :
    Gl.Window.t => (Reasongl.canvasT, Reasongl.audioContextT) =
    "%identity";
  [@bs.send]
  external getContext : (Reasongl.canvasT, string, 'options) => Gl.contextT =
    "getContext";
  let cull_face = 2884;
  let init = (width, height) => {
    let window = Gl.Window.init(~screen="main", ~argv=[||]);
    Gl.Window.setWindowSize(~window, ~width, ~height);
    /*let context = Gl.Window.getContext(window);*/
    let (canvasEl, _) = __destructureWindowHack(window);
    let context =
      getContext(
        canvasEl,
        "webgl",
        {"preserveDrawingBuffer": true, "antialias": true, "stencil": true}
      );
    glEnable(context, cull_face);
    Gl.viewport(~context, ~x=0, ~y=0, ~width, ~height);
    {
      window,
      context,
      width,
      height,
      gpuState: {
        curProg: 0,
        curVertex: 0,
        curIndex: 0,
        curTextures: [||]
      },
      keyboard: {
        keyCode: Gl.Events.Nothing
      },
      deltaTime: 0.0,
      elapsed: 0.0
    };
  };
  let resize = (self, width, height) => {
    Gl.Window.setWindowSize(~window=self.window, ~width, ~height);
    Gl.viewport(~context=self.context, ~x=0, ~y=0, ~width, ~height);
    self.width = width;
    self.height = height;
  };
  let run = (width, height, setup, draw, ~keyPressed=?, ~resize=?, ()) => {
    let canvas = init(width, height);
    let userState = ref(setup(canvas));
    /* Start render loop */
    Gl.render(
      ~window=canvas.window,
      ~displayFunc=
        f => {
          canvas.deltaTime = f /. 1000.;
          canvas.elapsed = canvas.elapsed +. canvas.deltaTime;
          userState := draw(userState^, canvas);
        },
      ~windowResize=
        () =>
          switch resize {
          | Some(resize) => resize(userState^)
          | None => ()
          },
      ~keyDown=
        (~keycode, ~repeat) => {
          canvas.keyboard.keyCode = keycode;
          if (! repeat) {
            switch keyPressed {
            | Some(keyPressed) => userState := keyPressed(userState^, canvas)
            | None => ()
            };
          };
        },
      ~keyUp=(~keycode) => (),
      /* Need this to trigger cleaning of keyes pressed
         and repeat marked */
      ()
    );
  };
  [@bs.send]
  external _colorMask :
    (Gl.contextT, bool, bool, bool, bool) => unit =
    "colorMask";
  [@bs.send]
  external _depthMask : (Gl.contextT, bool) => unit = "depthMask";
  let colorMask = (context, r, g, b, a) => {
    _colorMask(context, r, g, b, a);
  };
  let depthMask = (context, flag) =>
    _depthMask(context, flag);
  let clear = (canvas, r, g, b, a) => {
    Gl.clearColor(~context=canvas.context, ~r, ~g, ~b, ~a);
    Gl.clear(~context=canvas.context, ~mask=Constants.color_buffer_bit);
  };
  let clearStencil = canvas =>
    Gl.clear(~context=canvas.context, ~mask=Constants.stencil_buffer_bit);
  let setFramebuffer = (canvas, frameBuffer: FrameBuffer.inited) => {
    FrameBuffer._bindFramebuffer(
      ~context=canvas.context,
      FrameBuffer.frameBufferC,
      Js.Nullable.return(frameBuffer.frameBufferRef)
    );
    Gl.viewport(
      ~context=canvas.context,
      ~x=0,
      ~y=0,
      ~width=frameBuffer.width,
      ~height=frameBuffer.height
    );
  };
  let clearFramebuffer = canvas => {
    FrameBuffer._bindFramebuffer(
      ~context=canvas.context,
      FrameBuffer.frameBufferC,
      Js.Nullable.null
    );
    Gl.viewport(
      ~context=canvas.context,
      ~x=0,
      ~y=0,
      ~width=canvas.width,
      ~height=canvas.height
    );
  };
  [@bs.send]
  external enableVertexAttribArray : (Gl.contextT, Gl.attributeT) => unit =
    "enableVertexAttribArray";
  /* todo: currently not used, not sure if it works */
  let drawVertices =
      (canvas, program: Program.inited, vertexBuffer: VertexBuffer.inited) => {
    let context = canvas.context;
    let gpuState = canvas.gpuState;
    if (gpuState.curProg != program.rId) {
      Gl.useProgram(~context, program.programRef);
      List.iter(
        (attrib: VertexAttrib.inited) => {
          VertexAttrib.setPointer(attrib, context);
          enableVertexAttribArray(context, attrib.loc);
        },
        program.attribs
      );
      gpuState.curProg = program.rId;
    };
    if (gpuState.curVertex != vertexBuffer.rId) {
      glBindBuffer(context, Constants.array_buffer, vertexBuffer.bufferRef);
      gpuState.curVertex = vertexBuffer.rId;
    };
    Gl.drawArrays(
      ~context,
      ~mode=Constants.triangles,
      ~count=vertexBuffer.count,
      ~first=0
    );
  };
  /* Putting some here to avoid currying in
     this function that runs very often */
  [@bs.send]
  external useProgram : (Gl.contextT, Gl.programT) => unit = "useProgram";
  [@bs.send]
  external activeTexture : (Gl.contextT, int) => unit = "activeTexture";
  [@bs.send]
  external bindTexture : (Gl.contextT, int, Gl.textureT) => unit =
    "bindTexture";
  /* context, mode, count, type, offset */
  [@bs.send]
  external drawElements : (Gl.contextT, int, int, int, int) => unit =
    "drawElements";
  let drawIndexes =
      (
        canvas,
        program: Program.inited,
        vertexBuffer: VertexBuffer.inited,
        indexBuffer: IndexBuffer.inited,
        textures: array(ProgramTexture.inited)
      ) => {
    let context = canvas.context;
    let gpuState = canvas.gpuState;
    let switchedProgram =
      if (gpuState.curProg != program.rId) {
        useProgram(context, program.programRef);
        gpuState.curProg = program.rId;
        true;
      } else {
        false;
      };
    /* Set uniforms */
    List.iter(
      uniform => Uniform.valueToGpu(uniform, context),
      program.uniforms
    );
    /* Vertex buffer */
    let switchedBuffer =
      if (gpuState.curVertex != vertexBuffer.rId) {
        glBindBuffer(context, Constants.array_buffer, vertexBuffer.bufferRef);
        if (vertexBuffer.update) {
          VertexBuffer.updateGpuData(vertexBuffer, context);
        };
        gpuState.curVertex = vertexBuffer.rId;
        true;
      } else {
        false;
      };
    /* Not sure of the required logic here, when
       the pointers need to be set, it does seem to be
       needed after changing buffer */
    if (switchedProgram || switchedBuffer) {
      List.iter(
        (attrib: VertexAttrib.inited) => {
          VertexAttrib.setPointer(attrib, context);
          enableVertexAttribArray(context, attrib.loc);
        },
        program.attribs
      );
    };
    /* Index buffer */
    if (gpuState.curIndex != indexBuffer.rId) {
      glBindBuffer(
        context,
        Constants.element_array_buffer,
        indexBuffer.elBufferRef
      );
      if (indexBuffer.update) {
        IndexBuffer.updateGpuData(indexBuffer);
      };
      gpuState.curIndex = indexBuffer.rId;
    };
    /* Textures */
    let tex0 = Constants.texture0;
    let currTexLength = Array.length(gpuState.curTextures);
    gpuState.curTextures =
      Array.mapi(
        (i, pInit: ProgramTexture.inited) => {
          let wasBound =
            if (i > currTexLength - 1 || gpuState.curTextures[i] != pInit.rId) {
              Uniform.uniform1i(context, pInit.uniformRef, i);
              activeTexture(context, tex0 + i);
              bindTexture(context, Constants.texture_2d, pInit.texture.texRef);
              true;
            } else {
              false;
            };
          if (pInit.texture.update) {
            if (! wasBound) {
              activeTexture(context, tex0 + i);
              bindTexture(context, Constants.texture_2d, pInit.texture.texRef);
            };
            Texture.updateGpuData(pInit.texture);
          };
          pInit.rId;
        },
        textures
      );
    /* Draw elements */
    drawElements(
      context,
      Constants.triangles,
      indexBuffer.count,
      Constants.unsigned_short,
      0
    );
    switch (Error.getErrorString(context)) {
    | None => ()
    | Some(error) =>
      Js.log(error);
      [%debugger];
    };
  };
};

module DrawState = {
  type t = {
    program: Program.inited,
    vertexBuffer: VertexBuffer.inited,
    indexBuffer: option(IndexBuffer.inited),
    textures: array(ProgramTexture.inited)
  };
  let init = (context, program, vertexes, indexes, textures, gpuState) => {
    let pInited = Program.init(program, context);
    switch pInited {
    | Some(program) =>
      let iBuffer = VertexBuffer.init(vertexes, context, gpuState);
      let iIndexes =
        switch indexes {
        | Some(indices) => Some(IndexBuffer.init(indices, context, gpuState))
        | None => None
        };
      let iTextures =
        List.map(
          (programTex: ProgramTexture.t) => {
            let pInit =
              ProgramTexture.init(programTex, context, program.programRef);
            pInit;
          },
          textures
        );
      {
        program,
        vertexBuffer: iBuffer,
        indexBuffer: iIndexes,
        textures: Array.of_list(iTextures)
      };
    | None => failwith("Program creation failed")
    };
  };
  let fromInited = (program, vertexBuffer, indexBuffer, textures) => {
    program,
    vertexBuffer,
    indexBuffer,
    textures
  };
  let draw = (drawState, canvas) =>
    switch drawState.indexBuffer {
    | Some(indexBuffer) =>
      Canvas.drawIndexes(
        canvas,
        drawState.program,
        drawState.vertexBuffer,
        indexBuffer,
        drawState.textures
      )
    | None =>
      Canvas.drawVertices(canvas, drawState.program, drawState.vertexBuffer)
    };
};
