let vertSource = () => {|
        precision mediump float;
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat3 layout;
        varying vec3 vPos;
        varying vec3 vScreenPos;
        varying vec3 vNormal;
        void main() {
            // Z currently not affected by layout
            vec3 pos2 = vec3((vec3(position.xy, 1.0) * layout).xy, position.z);
            vPos = position;
            vScreenPos = pos2;
            vNormal = normal;
            gl_Position = vec4(pos2, 1.0);
        }
    |};

let fragSource = light => {
  let lightDecls = Light.ProgramLight.getFragVarDecls(light);
  let lightSrc = Light.ProgramLight.getLightFunction(light);
  {|
        precision mediump float;
        varying vec3 vPos;
        varying vec3 vScreenPos;
        varying vec3 vNormal;
        |}
  ++ lightDecls
  ++ {|

        |}
  ++ lightSrc
  ++ {|

        void main() {
            vec3 color = lighting(vPos, vScreenPos, vNormal);
            gl_FragColor = vec4(color, 0.2);
        }
    |};
};

module StoreSpec = {
  type hash = {light: Light.ProgramLight.hash};
  /* Currently only varies on light */
  type progType = Light.ProgramLight.t;
  let getHash = light => {light: Light.ProgramLight.makeHash(light)};
  let createProgram = light => {
    let vs = vertSource();
    let fs = fragSource(light);
    Gpu.(
      Scene.makeProgram(
        ~vertShader=Shader.make(vs),
        ~fragShader=Shader.make(fs),
        ~defaultUniforms=Light.ProgramLight.getUniforms(light),
        ~attribs=[
          VertexAttrib.make("position", GlType.Vec3f),
          VertexAttrib.make("normal", GlType.Vec3f)
        ],
        ()
      )
    );
  };
  let tblSize = 3;
};

module Programs = ProgramStore.Make(StoreSpec);

let make =
    (
      vo,
      ~size=Scene.Dimensions(Scene.Scale(1.0), Scene.Scale(1.0)),
      ~light,
      ()
    ) =>
  Scene.makeNode(
    ~program=Programs.getProgram(light),
    ~cls="node3d",
    ~size,
    ~uniforms=Light.ProgramLight.getUniforms(light),
    ~transparent=true,
    ~vo,
    ()
  );
