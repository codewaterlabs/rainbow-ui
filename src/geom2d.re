module Rect = {
  type t = {
    mutable x: float,
    mutable y: float,
    mutable w: float,
    mutable h: float
  };
  let make = (x, y, w, h) => {x, y, w, h};
  let zeros = () => {x: 0.0, y: 0.0, w: 0.0, h: 0.0};
  let equals = (self, rect) =>
    self.x == rect.x
    && self.y == rect.y
    && self.w == rect.w
    && self.h == rect.h;
  let contains = (self, rect) =>
    self.x <= rect.x
    && self.y <= rect.y
    && self.x
    +. self.w >= rect.x
    +. rect.w
    && self.y
    +. self.h >= rect.y
    +. rect.h;
  let intersects = (self, rect) =>
    rect.x <= self.x
    +. self.w
    && self.x <= rect.x
    +. rect.w
    && rect.y <= self.y
    +. self.h
    && self.y <= rect.y
    +. rect.h;
  let bounding = (self, rect) => {
    let srx = self.x +. self.w;
    let sby = self.y +. self.h;
    let rrx = rect.x +. rect.w;
    let rby = rect.y +. rect.h;
    let rx = srx > rrx ? srx : rrx;
    let by = sby > rby ? sby : rby;
    if (self.x < rect.x) {
      if (self.y < rect.y) {
        make(self.x, self.y, rx -. self.x, by -. self.y);
      } else {
        make(self.x, rect.y, rx -. self.x, by -. rect.y);
      };
    } else if (self.y < rect.y) {
      make(rect.x, self.y, rx -. rect.x, by -. self.y);
    } else {
      make(rect.x, rect.y, rx -. rect.x, by -. rect.y);
    };
  };
};
