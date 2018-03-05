type lightColor =
  | StaticColor(Color.t)
  | DynamicColor(Scene.sceneUniform);

type lightPos =
  | StaticPos(Data.Vec3.t)
  | DynamicPos(Scene.sceneUniform);

type lightDir =
  | StaticDir(Data.Vec3.t)
  | DynamicDir(Scene.sceneUniform);

type coordSystem =
  | ScreenCoords
  | LocalCoords;

type fragSource = {
  statements: string,
  addend: string
};

/* Hashable values to index scenePrograms */
let lightColorHash = (lightColor: lightColor) =>
  switch lightColor {
  | StaticColor(color) => Some(color)
  | DynamicColor(_) => None
  };

let lightPosHash = (lightPos: lightPos) =>
  switch lightPos {
  | StaticPos(pos) => Some(pos)
  | DynamicPos(_) => None
  };

let lightDirHash = (lightDir: lightDir) =>
  switch lightDir {
  | StaticDir(dir) => Some(dir)
  | DynamicDir(_) => None
  };

module PointLight = {
  type t = {
    pos: lightPos,
    color: lightColor,
    coords: coordSystem,
    factor: float,
    specular: int
  };
  type hash = {
    /* Pos static or uniform (encoded as None) */
    hLightPos: option(Data.Vec3.t),
    /* Static or uniform (None) */
    hColor: option(Color.t),
    hCoords: coordSystem,
    hFactor: float,
    hSpecular: int
  };
  let makeHash = (self: t) : hash => {
    hLightPos: lightPosHash(self.pos),
    hColor: lightColorHash(self.color),
    hCoords: self.coords,
    hFactor: self.factor,
    hSpecular: self.specular
  };
  let make =
      (
        ~pos,
        ~color=StaticColor(Color.white()),
        ~coords=ScreenCoords,
        ~factor=0.6,
        ~specular=12,
        ()
      ) => {
    pos,
    color,
    coords,
    factor,
    specular
  };
  let getUniforms = (self, i) => {
    let istr = string_of_int(i);
    let uniforms =
      switch self.pos {
      | DynamicPos(u) => [("uPointPos" ++ istr, u)]
      | _ => []
      };
    let uniforms =
      switch self.color {
      | DynamicColor(u) => [("uPointColor" ++ istr, u), ...uniforms]
      | _ => uniforms
      };
    uniforms;
  };
  let getFragVarDecls = (self, i) => {
    let istr = string_of_int(i);
    let dir =
      switch self.pos {
      | DynamicPos(_) => "uniform vec3 uPointPos" ++ istr ++ ";\n"
      | _ => ""
      };
    let color =
      switch self.color {
      | DynamicColor(_) => "uniform vec3 uPointColor" ++ istr ++ ";\n"
      | StaticColor(_) => ""
      };
    dir ++ color;
  };
  let getLightFuncSource = (self, i, camera: Camera.t) => {
    let istr = string_of_int(i);
    let lightPoint =
      switch self.pos {
      | StaticPos(v) => Data.Vec3.toGlsl(v)
      | DynamicPos(_) => "uPointPos" ++ istr
      };
    let point =
      switch self.coords {
      | ScreenCoords => "screenP"
      | LocalCoords => "localP"
      };
    let color =
      switch self.color {
      | DynamicColor(_) => "uPointColor" ++ istr
      | StaticColor(c) => Data.Vec3.toGlsl(c)
      };
    /* todo: some should be done in vertex shader:
       lightDir and cameraDir, which then should be
       normalized in fragment shader */
    let pointDir = "pointDir" ++ istr;
    let pointDirStm =
      "vec3 "
      ++ pointDir
      ++ " = normalize("
      ++ lightPoint
      ++ " - "
      ++ point
      ++ ");";
    let dot =
      "max(dot("
      ++ pointDir
      ++ ", normal), 0.0) * "
      ++ color
      ++ " * "
      ++ string_of_float(self.factor);
    if (self.specular == 0) {
      {statements: pointDirStm, addend: dot};
    } else {
      let lightDir = "lightDir" ++ istr;
      let lightDirStm =
        "vec3 "
        ++ lightDir
        ++ " = normalize("
        ++ point
        ++ " - "
        ++ lightPoint
        ++ ");";
      let reflLight = "reflect(" ++ lightDir ++ ", normal)";
      let cameraDir =
        "normalize(" ++ Data.Vec3.toGlsl(camera.pos) ++ " - " ++ point ++ ")";
      let specular =
        "pow(clamp(dot("
        ++ cameraDir
        ++ ", "
        ++ reflLight
        ++ "), 0.0, 1.0), "
        ++ string_of_int(self.specular)
        ++ ".0)";
      let specularColor = "vec3(1.0, 1.0, 1.0)";
      let specularAddend = specular ++ " * " ++ specularColor;
      {
        statements: pointDirStm ++ lightDirStm,
        addend: specularAddend ++ " + " ++ dot
      };
    };
  };
  /* Light function for only this pointLight */
  let getLightFunction = (self, camera: Camera.t) => {
    let part = getLightFuncSource(self, 0, camera);
    "vec3 lighting(vec3 localP, vec3 screenP, vec3 normal) {\n"
    ++ part.statements
    ++ "return pow("
    ++ part.addend
    ++ ", vec3(1.0/2.2));\n"
    ++ "}\n";
  };
};

module Directional = {
  type t = {
    dir: lightDir,
    color: lightColor,
    coords: coordSystem,
    factor: float,
    specular: int
  };
  type hash = {
    hDir: option(Data.Vec3.t),
    hColor: option(Color.t),
    hCoords: coordSystem,
    hFactor: float,
    hSpecular: int
  };
  let makeHash = self => {
    hDir: lightDirHash(self.dir),
    hColor: lightColorHash(self.color),
    hCoords: self.coords,
    hFactor: self.factor,
    hSpecular: self.specular
  };
  let make =
      (
        ~dir,
        ~color=StaticColor(Color.white()),
        ~coords=ScreenCoords,
        ~factor=0.4,
        ~specular=16,
        ()
      ) => {
    dir,
    color,
    coords,
    factor,
    specular
  };
  let getUniforms = self => {
    let uniforms =
      switch self.dir {
      | DynamicDir(u) => [("uDir", u)]
      | StaticDir(_) => []
      };
    let uniforms =
      switch self.color {
      | DynamicColor(u) => [("uDirColor", u)]
      | StaticColor(_) => uniforms
      };
    uniforms;
  };
  let getFragVarDecls = self => {
    let dir =
      switch self.dir {
      | DynamicDir(_) => "uniform vec3 uDir;\n"
      | StaticDir(_) => ""
      };
    let color =
      switch self.color {
      | DynamicColor(_) => "uniform vec3 uDirColor;\n"
      | StaticColor(_) => ""
      };
    dir ++ color;
  };
  let getLightFuncSource = self => {
    let dir =
      switch self.dir {
      | StaticDir(v) => Data.Vec3.toGlsl(v)
      | DynamicDir(_) => "uDir"
      };
    let color =
      switch self.color {
      | DynamicColor(_) => "uDirColor"
      | StaticColor(c) => Data.Vec3.toGlsl(c)
      };
    let dot =
      "max(dot("
      ++ dir
      ++ ", normal), 0.0) * "
      ++ color
      ++ " * "
      ++ string_of_float(self.factor);
    {statements: "", addend: dot};
  };
};

module ProgramLight = {
  type t = {
    dir: Directional.t,
    points: list(PointLight.t),
    camera: Camera.t
  };
  /* Camera is assumed to be uniform (not currently the case) */
  type hash = {
    hDir: Directional.hash,
    hPoints: list(PointLight.hash)
  };
  let makeHash = self => {
    hDir: Directional.makeHash(self.dir),
    hPoints: List.map(point => PointLight.makeHash(point), self.points)
  };
  let make = (dir, points, camera) => {dir, points, camera};
  let default = () => {
    let dirLight =
      Directional.make(~dir=StaticDir(Data.Vec3.make(0.4, 0.3, 0.3)), ());
    let pointLight =
      PointLight.make(~pos=StaticPos(Data.Vec3.make(0.0, -0.4, 2.0)), ());
    let camera = Camera.make(Data.Vec3.make(0.0, 0.4, 4.0));
    make(dirLight, [pointLight], camera);
  };
  let getUniforms = self => {
    let uniforms =
      List.concat(
        List.mapi((i, p) => PointLight.getUniforms(p, i), self.points)
      );
    let uniforms = List.append(Directional.getUniforms(self.dir), uniforms);
    uniforms;
  };
  let getVertVarDecls = _self => ();
  let getFragVarDecls = self => {
    let (_, decls) =
      List.fold_left(
        ((i, decls), p) => (i + 1, decls ++ PointLight.getFragVarDecls(p, i)),
        (0, ""),
        self.points
      );
    Directional.getFragVarDecls(self.dir) ++ decls;
  };
  let getLightFunction = self => {
    /* Bit simplistic now, expecting expressions
       from the parts */
    let (_, parts) =
      List.fold_left(
        ((i, parts), p) => (
          i + 1,
          [PointLight.getLightFuncSource(p, i, self.camera), ...parts]
        ),
        (0, []),
        self.points
      );
    let parts = [Directional.getLightFuncSource(self.dir), ...parts];
    let (statements, addends) =
      List.fold_left(
        ((statements, addends), part) => (
          statements ++ part.statements,
          [part.addend, ...addends]
        ),
        ("", []),
        parts
      );
    "vec3 lighting(vec3 localP, vec3 screenP, vec3 normal) {\n"
    ++ statements
    ++ "return pow("
    ++ String.concat(" + ", addends)
    ++ ", vec3(1.0/2.2));\n"
    ++ "}\n";
  };
};
