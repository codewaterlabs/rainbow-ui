module Point = {
  type t = {
    x: float,
    y: float,
    z: float
  };
  let make = (x, y, z) => {x, y, z};
  /* Normal from p1 to p2 and p1 to p3 */
  let normal = (p1, p2, p3) => {
    let v1 = Data.Vec3.make(p2.x -. p1.x, p2.y -. p1.y, p2.z -. p1.z);
    let v2 = Data.Vec3.make(p3.x -. p1.x, p3.y -. p1.y, p3.z -. p1.z);
    Data.Vec3.(normalize(cross(v1, v2)));
  };
};

/* "Quadrilateral", four points defining an area */
module Quad = {
  type t = {
    bl: Point.t,
    br: Point.t,
    tr: Point.t,
    tl: Point.t
  };
  let make = (bl, br, tr, tl) => {bl, tl, tr, br};
  /* Temp, could have fromRect or toQuad instead */
  let makeRect = (x, y, w, h, z) =>
    make(
      Point.make(x, y -. h, z),
      Point.make(x +. w, y -. h, z),
      Point.make(x +. w, y, z),
      Point.make(x, y, z)
    );
  let normal = q =>
    /* Make vector from bottom left to bottom right
       and bottom left to top left and
       take cross product of their normalized values */
    Point.normal(q.bl, q.br, q.tl);
};

/* I don't know the terms
   Based on two quadrilaterals, this
   models a solid area, filling
   out the connecting planes between
   them */
module AreaBetweenQuads = {
  type t = {
    back: Quad.t,
    front: Quad.t
  };
  let make = (back, front) => {back, front};
  let createVertexData = self => {
    let q1 = self.front;
    let q2 = self.back;
    /* Calculate normals */
    let fNorm = Quad.normal(q1);
    let bkNorm = Quad.normal(q1);
    let rNorm = Point.normal(q1.br, q2.br, q1.tr);
    let bNorm = Point.normal(q1.bl, q2.bl, q1.br);
    let lNorm = Point.normal(q1.tl, q2.tl, q1.bl);
    let tNorm = Point.normal(q1.tr, q2.tr, q1.tl);
    /* Here it goes front face, right face,
       bottom face, left face, top face,
       back face */
    /* Since we want normals, some coordinate
       entries are duplicated
       Setting points in ccw order */
    [|
      /* Front face */
      q1.bl.x,
      q1.bl.y,
      q1.bl.z,
      fNorm[0],
      fNorm[1],
      fNorm[2],
      q1.br.x,
      q1.br.y,
      q1.br.z,
      fNorm[0],
      fNorm[1],
      fNorm[2],
      q1.tr.x,
      q1.tr.y,
      q1.tr.z,
      fNorm[0],
      fNorm[1],
      fNorm[2],
      q1.tl.x,
      q1.tl.y,
      q1.tl.z,
      fNorm[0],
      fNorm[1],
      fNorm[2],
      /* Right face, right side of q1 and q2 */
      q1.br.x,
      q1.br.y,
      q1.br.z,
      rNorm[0],
      rNorm[1],
      rNorm[2],
      q2.br.x,
      q2.br.y,
      q2.br.z,
      rNorm[0],
      rNorm[1],
      rNorm[2],
      q2.tr.x,
      q2.tr.y,
      q2.tr.z,
      rNorm[0],
      rNorm[1],
      rNorm[2],
      q1.tr.x,
      q1.tr.y,
      q1.tr.z,
      rNorm[0],
      rNorm[1],
      rNorm[2],
      /* Bottom face */
      q2.bl.x,
      q2.bl.y,
      q2.bl.z,
      bNorm[0],
      bNorm[1],
      bNorm[2],
      q2.br.x,
      q2.br.y,
      q2.br.z,
      bNorm[0],
      bNorm[1],
      bNorm[2],
      q1.br.x,
      q1.br.y,
      q1.br.z,
      bNorm[0],
      bNorm[1],
      bNorm[2],
      q1.bl.x,
      q1.bl.y,
      q1.bl.z,
      bNorm[0],
      bNorm[1],
      bNorm[2],
      /* Left face, left side of q2 and q1 */
      q2.bl.x,
      q2.bl.y,
      q2.bl.z,
      lNorm[0],
      lNorm[1],
      lNorm[2],
      q1.bl.x,
      q1.bl.y,
      q1.bl.z,
      lNorm[0],
      lNorm[1],
      lNorm[2],
      q1.tl.x,
      q1.tl.y,
      q1.tl.z,
      lNorm[0],
      lNorm[1],
      lNorm[2],
      q2.tl.x,
      q2.tl.y,
      q2.tl.z,
      lNorm[0],
      lNorm[1],
      lNorm[2],
      /* Top face */
      q2.tr.x,
      q2.tr.y,
      q2.tr.z,
      tNorm[0],
      tNorm[1],
      tNorm[2],
      q2.tl.x,
      q2.tl.y,
      q2.tl.z,
      tNorm[0],
      tNorm[1],
      tNorm[2],
      q1.tl.x,
      q1.tl.y,
      q1.tl.z,
      tNorm[0],
      tNorm[1],
      tNorm[2],
      q1.tr.x,
      q1.tr.y,
      q1.tr.z,
      tNorm[0],
      tNorm[1],
      tNorm[2],
      /* Back face */
      q2.bl.x,
      q2.bl.y,
      q2.bl.z,
      bkNorm[0],
      bkNorm[1],
      bkNorm[2],
      q2.tl.x,
      q2.tl.y,
      q2.tl.z,
      bkNorm[0],
      bkNorm[1],
      bkNorm[2],
      q2.tr.x,
      q2.tr.y,
      q2.tr.z,
      bkNorm[0],
      bkNorm[1],
      bkNorm[2],
      q2.br.x,
      q2.br.y,
      q2.br.z,
      bkNorm[0],
      bkNorm[1],
      bkNorm[2]
    |];
  };
  let makeVertexObject = (self, ~usage=Gpu.StaticDraw, ()) => {
    open Gpu;
    let vbuf =
      VertexBuffer.make(
        createVertexData(self),
        [("position", GlType.Vec3f), ("normal", GlType.Vec3f)],
        usage
      );
    let ibuf = IndexBuffer.make(IndexBuffer.makeQuadsData(6), usage);
    Scene.SVertexObject.make(vbuf, Some(ibuf));
  };
};
