module type ArrayBType = {
  type a;
  let defaultVal: unit => a;
  let isStaticVal: bool;
};

module Make = (Typed: ArrayBType) => {
  type t = {
    mutable data: array(Typed.a),
    mutable bufferSize: int,
    mutable size: int
  };
  let staticVal =
    if (Typed.isStaticVal) {
      Some(Typed.defaultVal());
    } else {
      None;
    };
  let make = bufferSize => {
    data:
      switch staticVal {
      | Some(staticVal) => Array.make(bufferSize, staticVal)
      | None => Array.init(bufferSize, (_) => Typed.defaultVal())
      },
    bufferSize,
    size: 0
  };
  let grow = (self, newSize) => {
    switch staticVal {
    | Some(staticVal) =>
      self.data =
        Array.init(newSize, i =>
          if (i < self.bufferSize) {
            self.data[i];
          } else {
            staticVal;
          }
        )
    | None =>
      self.data =
        Array.init(newSize, i =>
          if (i < self.bufferSize) {
            self.data[i];
          } else {
            Typed.defaultVal();
          }
        )
    };
    self.bufferSize = newSize;
  };
  let ensureSize = (self, size) =>
    if (self.bufferSize < size) {
      let size = size + size / 4;
      grow(self, size);
    };
  let push = (self, element) => {
    if (self.size >= self.bufferSize) {
      grow(self, self.bufferSize * 2);
    };
    self.data[self.size] = element;
    self.size = self.size + 1;
  };
  let push2 = (self, el1, el2) => {
    if (self.size + 2 > self.bufferSize) {
      grow(self, self.bufferSize * 2);
    };
    self.data[self.size] = el1;
    self.data[self.size + 1] = el2;
    self.size = self.size + 2;
  };
  let push3 = (self, el1, el2, el3) => {
    if (self.size + 3 > self.bufferSize) {
      grow(self, self.bufferSize * 2);
    };
    self.data[self.size] = el1;
    self.data[self.size + 1] = el2;
    self.data[self.size + 2] = el3;
    self.size = self.size + 3;
  };
  let push4 = (self, el1, el2, el3, el4) => {
    if (self.size + 4 > self.bufferSize) {
      grow(self, self.bufferSize * 2);
    };
    self.data[self.size] = el1;
    self.data[self.size + 1] = el2;
    self.data[self.size + 2] = el3;
    self.data[self.size + 3] = el4;
    self.size = self.size + 4;
  };
  let toArray = self => Array.sub(self.data, 0, self.size);
};

module ArrayBInt =
  Make(
    {
      type a = int;
      let defaultVal = () => 0;
      let isStaticVal = true;
    }
  );
