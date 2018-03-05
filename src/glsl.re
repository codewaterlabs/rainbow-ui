type glslType =
  | GlslFloat
  | GlslVec2f
  | GlslVec3f
  | GlslVec4f
  | GlslMat3f
  | GlslMat4f;

module GlslType = {
  let toGlsl = tpe =>
    switch tpe {
    | GlslFloat => "float"
    | GlslVec2f => "vec2"
    | GlslVec3f => "vec3"
    | GlslVec4f => "vec4"
    | GlslMat3f => "mat3"
    | GlslMat4f => "mat4"
    };
};

module GlslUniform = {
  type t = {
    name: string,
    tpe: glslType
  };
  let make = (name, tpe) => {name, tpe};
  let toGlsl = self =>
    "uniform " ++ GlslType.toGlsl(self.tpe) ++ " " ++ self.name ++ ";";
};

module GlslVarying = {
  type t = {
    name: string,
    tpe: glslType
  };
  let make = (name, tpe) => {name, tpe};
  let toGlsl = self =>
    "varying " ++ GlslType.toGlsl(self.tpe) ++ " " ++ self.name ++ ";";
};

type glslExpr =
  | GlslVar(string)
  | GlslVec3fLit(Data.Vec3.t)
  | GlslVec4fLit(Data.Vec4.t)
  | GlslFloatLit(float);

module GlslExpr = {
  let toGlsl = expr =>
    switch expr {
    | GlslVar(name) => name
    | GlslVec3fLit(v3) => Data.Vec3.toGlsl(v3)
    | GlslVec4fLit(v4) => Data.Vec4.toGlsl(v4)
    | GlslFloatLit(f) => string_of_float(f)
    };
};
