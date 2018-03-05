type animTarget('state) =
  | AnimUniform(Scene.sceneUniform)
  | AnimNodeUniform(Scene.node('state), string);

/* Anim value, from to */
type animValue =
  | AnimFloat(float, float)
  | AnimVec2(Data.Vec2.t, Data.Vec2.t)
  | AnimVec3(Data.Vec3.t, Data.Vec3.t);

let pi = 4.0 *. atan(1.0);

let halfPi = pi /. 2.0;

/* Returns 0.0 to 1.0 based on duration and given elapsed */
let getEasingFunction = (easing, duration) =>
  switch easing {
  | Scene.Linear => (elapsed => elapsed /. duration)
  | Scene.SineOut => (elapsed => sin(elapsed /. duration *. halfPi))
  | Scene.SineIn => (
      elapsed => sin(elapsed /. duration *. halfPi -. halfPi) +. 1.0
    )
  | Scene.SineInOut => (
      elapsed => sin(elapsed /. duration *. pi -. halfPi) *. 0.5 +. 0.5
    )
  };

let anim =
    (
      animTarget,
      animValue,
      ~key=?,
      ~easing=Scene.SineOut,
      ~duration=2.0,
      ~frameInterval=1,
      ~next=?,
      ~onDone=?,
      ()
    ) => {
  let easingFunc = getEasingFunction(easing, duration);
  let (onFrame, setLast) =
    switch animValue {
    | AnimFloat(from, last) =>
      let remaining = last -. from;
      switch animTarget {
      | AnimUniform(u) => (
          [@bs]
          (
            (scene, anim: Scene.anim('state)) =>
              Scene.UFloat.set(
                scene,
                u,
                from +. remaining *. easingFunc(anim.elapsed)
              )
          ),
          [@bs] (scene => Scene.UFloat.set(scene, u, last))
        )
      | AnimNodeUniform(n, u) => (
          [@bs]
          (
            (scene, anim: Scene.anim('state)) =>
              Scene.setUniformFloat(
                scene,
                n,
                u,
                from +. remaining *. easingFunc(anim.elapsed)
              )
          ),
          [@bs] (scene => Scene.setUniformFloat(scene, n, u, last))
        )
      };
    | AnimVec2(from, last) =>
      let fromX = Data.Vec2.getX(from);
      let fromY = Data.Vec2.getY(from);
      let remainingX = Data.Vec2.getX(last) -. fromX;
      let remainingY = Data.Vec2.getY(last) -. fromY;
      switch animTarget {
      | AnimUniform(u) => (
          [@bs]
          (
            (scene, anim: Scene.anim('state)) => {
              let easing = easingFunc(anim.elapsed);
              Scene.UVec2f.set(
                scene,
                u,
                Data.Vec2.make(
                  fromX +. remainingX *. easing,
                  fromY +. remainingY *. easing
                )
              );
            }
          ),
          [@bs] (scene => Scene.UVec2f.set(scene, u, last))
        )
      | AnimNodeUniform(n, u) => (
          [@bs]
          (
            (scene, anim: Scene.anim('state)) => {
              let easing = easingFunc(anim.elapsed);
              Scene.setUniformVec2f(
                scene,
                n,
                u,
                Data.Vec2.make(
                  fromX +. remainingX *. easing,
                  fromY +. remainingY *. easing
                )
              );
            }
          ),
          [@bs] (scene => Scene.setUniformVec2f(scene, n, u, last))
        )
      };
    | AnimVec3(from, last) =>
      let fromX = Data.Vec3.getX(from);
      let fromY = Data.Vec3.getY(from);
      let fromZ = Data.Vec3.getZ(from);
      let remainingX = Data.Vec3.getX(last) -. fromX;
      let remainingY = Data.Vec3.getY(last) -. fromY;
      let remainingZ = Data.Vec3.getZ(last) -. fromZ;
      switch animTarget {
      | AnimUniform(u) => (
          [@bs]
          (
            (scene, anim: Scene.anim('state)) => {
              let easing = easingFunc(anim.elapsed);
              Scene.UVec3f.set(
                scene,
                u,
                Data.Vec3.make(
                  fromX +. remainingX *. easing,
                  fromY +. remainingY *. easing,
                  fromZ +. remainingZ *. easing
                )
              );
            }
          ),
          [@bs] (scene => Scene.UVec3f.set(scene, u, last))
        )
      | AnimNodeUniform(n, u) => (
          [@bs]
          (
            (scene, anim: Scene.anim('state)) => {
              let easing = easingFunc(anim.elapsed);
              Scene.setUniformVec3f(
                scene,
                n,
                u,
                Data.Vec3.make(
                  fromX +. remainingX *. easing,
                  fromY +. remainingY *. easing,
                  fromZ +. remainingZ *. easing
                )
              );
            }
          ),
          [@bs] (scene => Scene.setUniformVec3f(scene, n, u, last))
        )
      };
    };
  Scene.makeAnim(
    onFrame,
    setLast,
    duration,
    ~key?,
    ~next?,
    ~frameInterval,
    ~onDone?,
    ()
  );
};
