let sdfDist = (width, height, rounding) =>
  {|
    float rounding = |}
  ++ string_of_float(rounding)
  ++ {|;
    float boxWidth = |}
  ++ string_of_float(width -. rounding)
  ++ {|;
    float boxHeight =|}
  ++ string_of_float(height -. rounding)
  ++ {|;
    float boxDepth = 0.2;
    float box = length(max(abs(point) - vec3(boxWidth, boxHeight, boxDepth), vec3(0.0, 0.0, 0.0))) - rounding;
    return box;
|};

let makeNode = (~width=1.0, ~height=1.0, ~color=?, ~lighting, children) => {
  let aspect = 12.0 /. 26.0;
  let model = Data.Mat3.scale(1.0, 1.0 /. aspect);
  let sdfNode =
    SdfNode.make(
      sdfDist(1.0, 1.0 /. aspect, 0.08),
      SdfNode.ByModel,
      Some(Scene.UMat3f.mat(model)),
      lighting,
      ~width,
      ~height,
      ~color?,
      ~alphaLimit=0.0,
      ~opacity=0.5,
      ()
    );
  SdfNode.makeNode(sdfNode, ~aspect, ~children, ());
};
