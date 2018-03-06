/* Scene nodes maps roughly to "draw states", a program draw with
   uniforms and textures.
   They also provide layout capabilities.

   The nodes will have some layout uniforms available.
   */
type childLayout =
  | Horizontal
  | Vertical
  | Stacked;

type hAlign =
  | AlignCenter
  | AlignLeft
  | AlignRight;

type vAlign =
  | AlignTop
  | AlignMiddle
  | AlignBottom;

/* Scale of available space, or pixel value */
type dimension =
  | Scale(float)
  | ScreenScale(float)
  | Pixel(float);

type margin =
  | Margin(dimension)
  | MarginXY(dimension, dimension)
  | MarginRBLT(dimension, dimension, dimension, dimension);

/* Size by dimensions or aspect with best fit in container */
type blockSize =
  | Dimensions(dimension, dimension)
  | WidthRatio(dimension, float)
  | HeightRatio(dimension, float)
  | Aspect(float);

type easing =
  | Linear
  | SineOut
  | SineIn
  | SineInOut;

/* todo: Does this make "transparent" redundant?
   Are there other abstractions that are better fit? */
type blendFactor =
  | BlendAlpha
  | BlendOne;

let currNodeId = ref(0);

/* Not thread safe :) */
let nextNodeId = () => {
  let nodeId = currNodeId^;
  currNodeId := nodeId + 1;
  nodeId;
};

type fbufferConfig = {
  width: int,
  height: int
};

/* user defined state and update flags */
type t('s) = {
  state: 's,
  mutable queuedUpdates: list(int),
  nodesByKey: Hashtbl.t(string, node('s)),
  nodesByCls: Hashtbl.t(string, list(node('s))),
  canvas: Gpu.Canvas.t,
  root: node('s),
  mutable inited: bool,
  drawLists: Hashtbl.t(updateState, list(drawListItem('s))),
  drawListsDebug: option(drawListDebug('s)),
  initedLists: Hashtbl.t(updateState, bool),
  initedDeps: Hashtbl.t(int, bool),
  mutable loadingNodes: list(node('s)),
  mutable anims: list(anim('s)),
  fbuffer: Hashtbl.t(fbufferConfig, Gpu.FrameBuffer.inited),
  stencilDraw: StencilDraw.t,
  /* Update nodes keyed by node id/number,
     array is sized to cover all nodes */
  mutable updateNodes: array(option(updateListNode('s))),
  mutable updateRoot: option(updateListNode('s)),
  mutable hiddenNodes: list(int),
  /* Queue for uncreated drawstates that are
     requested to be precreated
     todo: Consider frame interval
     (look at benchmarks for creation) */
  mutable queuedDrawStates: list(node('s)),
  /* Queued dependencies, these could be prioritized
     over queued drawstates */
  mutable queuedDeps: list(int),
  /* Whether to process queued drawstates/deps.
     This can be activated when there is resource
     availability (maybe automatically?) */
  initQueued: bool
}
and drawListDebug('s) = {draw: DrawListDebug.t}
and updateState = {
  updateNodes: list(int),
  hiddenNodes: list(int)
}
and node('s) = {
  key: option(string),
  cls: option(string),
  id: int,
  mutable drawState: option(Gpu.DrawState.t),
  onUpdate: option([@bs] ((node('s), 's) => unit)),
  layout,
  /* Calced layout properties */
  cLayout : calcLayout,
  /* Rect in pixels */
  rect: Geom2d.Rect.t,
  outRect: Geom2d.Rect.t,
  scissorRect: Geom2d.Rect.t,
  /* Rect in screen -1.0 to 1.0 */
  screenRect: Geom2d.Rect.t,
  selfDraw: bool,
  transparent: bool,
  blendFactor,
  partialDraw: bool,
  mutable hidden: bool,
  mutable loading: bool,
  deps: list(node('s)),
  children: list(node('s)),
  vertShader: option(Gpu.Shader.t),
  fragShader: option(Gpu.Shader.t),
  program: option(sceneProgram),
  textureList: list(string),
  textures: Hashtbl.t(string, sceneTexture),
  texMats: Hashtbl.t(string, (sceneTexture, Gpu.uniform)),
  vo: option(sceneVertexObject),
  attribs: option(list(Gpu.VertexAttrib.t)),
  uniformList: list(string),
  uniforms: Hashtbl.t(string, sceneUniform),
  layoutUniform: option(Gpu.uniform),
  pixelSizeUniform: option(Gpu.uniform),
  elapsedUniform: option(Gpu.uniform),
  drawToTexture: option(Gpu.Texture.t),
  texTransUniform: option(Gpu.uniform),
  clearOnDraw: bool,
  mutable parent: option(node('s)),
  mutable scene: option(t('s))
}
and sceneProgramInited = {
  /* Contains locs of uniforms in the order
     of uniformList.
     These will be reused in the drawState
     of nodes using this program */
  iProgram: Gpu.Program.inited,
  /* Initialized default textures */
  iTexture: Hashtbl.t(string, Gpu.ProgramTexture.inited),
  /* Locs to uniform locations of textures,
     these will be reused in the drawState
     of nodes using this program */
  iTextureU: Hashtbl.t(string, Gpu.Gl.uniformT),
  /* Texture mat uniforms */
  iTextureMat: Hashtbl.t(string, Gpu.Gl.uniformT),
  iVo: option((Gpu.VertexBuffer.inited, option(Gpu.IndexBuffer.inited))),
  /* System uniforms */
  layoutUniform: option(Gpu.Gl.uniformT),
  pixelSizeUniform: option(Gpu.Gl.uniformT),
  elapsedUniform: option(Gpu.Gl.uniformT),
  texTransUniform: option(Gpu.Gl.uniformT)
}
/* Sceneprogram to allow multiple nodes using the
   same program while optionally providing/maintaining
   their own uniforms, vertices, indexes, textures
   (Although they could reference same items)
   The actual uniforms are kept here, and values
   from nodes are used when drawing
   The optimization gain I suppose is mostly
   from reduced initialization, compilation and
   in particular linking */
and sceneProgram = {
  mutable inited: option(sceneProgramInited),
  vertShader: Gpu.Shader.t,
  fragShader: Gpu.Shader.t,
  /* Attribs or default vo */
  attribs: list(Gpu.VertexAttrib.t),
  vo: option(sceneVertexObject),
  /* Uniforms may have a default provided in the program
     or may be required from the node */
  uniformList: list(string),
  defaultUniforms: Hashtbl.t(string, sceneUniform),
  requiredUniforms: Hashtbl.t(string, Gpu.GlType.t),
  /* Textures may have a default provided or
     may be required from the node */
  textureList: list(string),
  defaultTextures: Hashtbl.t(string, (sceneTexture, Gpu.ProgramTexture.t)),
  /* Required textures, and
     whether to expect a texTrans uniform */
  requiredTextures: Hashtbl.t(string, bool),
  /* Values for these come from the nodes,
     but we need reference to the uniform location */
  /* Slight optimization would be to just keep Gpu.Gl.uniformT here */
  layoutUniform: bool,
  pixelSizeUniform: bool,
  elapsedUniform: bool,
  texTransUniform: bool
}
/* Also consider assigning id's to these
   objects, then connecting id's to nodes */
and sceneUniform = {
  mutable nodes: list(int),
  uniform: Gpu.uniform
}
and sceneVertexObject = {
  mutable nodes: list(int),
  vertexBuffer: Gpu.VertexBuffer.t,
  indexBuffer: option(Gpu.IndexBuffer.t)
}
and sceneTexture = {
  mutable nodes: list(int),
  texture: Gpu.Texture.t,
  texNode: option(int),
  uniformMat: option(Gpu.uniform),
  offsetX: dimension,
  offsetY: dimension
}
and layout = {
  size: blockSize,
  padding: option(dimension),
  margin: option(margin),
  childLayout,
  spacing: option(dimension),
  hAlign,
  vAlign
}
and calcLayout = {
  mutable pWidth: float,
  mutable pHeight: float,
  mutable marginX1: float,
  mutable marginX2: float,
  mutable marginY1: float,
  mutable marginY2: float,
  mutable inWidth: float,
  mutable inHeight: float,
  mutable pXOffset: float,
  mutable pYOffset: float
}
and anim('s) = {
  animKey: option(string),
  onFrame: [@bs] ((t('s), anim('s)) => unit),
  setLast: [@bs] (t('s) => unit),
  onDone: option(t('s) => unit),
  duration: float,
  mutable elapsed: float,
  frameInterval: int,
  mutable numFrames: int,
  next: option(anim('s))
}
and updateStencil = {
  mutable rect: Geom2d.Rect.t,
  mutable active: bool
}
and updateListNode('s) = {
  /* Whether there is a full update triggered
     on the node */
  mutable update: bool,
  /* Whether any child is registered for update,
     to help traversal */
  mutable childUpdate: bool,
  /* Whether this is part of a dependency node */
  isDep: bool,
  /* Whether this is a node directly in the deps list aka dep root */
  isDepRoot: bool,
  /* If node is part of a stacked layout, previous stacked nodes */
  prevStacked: list(updateListNode('s)),
  /* Partitioned to prev and next stacked */
  mutable nextStacked: list(updateListNode('s)),
  /* Reference to the node */
  updNode: node('s),
  /* Rectangle of the node, this is referenced in
     updateRects and stencils, and the data may possibly
     be updated on resize
     todo: Not sure whether this should be option or
     not, maybe not. If checks are maybe a little more
     jit */
  mutable rect: option(Geom2d.Rect.t),
  /* Stencil rects/(shapes) from children.
     A childs stencil is referenced and flagged as active
     as the child is marked for update */
  mutable stencils: list(updateStencil),
  /* When visible, not transparent and not partialDraw, this is
     stencil rect that can be used by parents.
     This is referenced upwards in the tree
     so when toggling active, it will
     propagate */
  stencil: option(updateStencil),
  /* Rectangles coming from child drawings,
     here is a list of possible child rects,
     where the ones needed should be activated,
     then on traversal all active rects
     should be reconciled */
  mutable childRects: list(updateRect('s)),
  /* Rects upwards in the tree that are needed
     to redraw this node, the list will be
     expanded as needed */
  mutable parentRects: list(updateRect('s)),
  mutable updDeps: list(updateListNode('s)),
  mutable updChildren: list(updateListNode('s)),
  parent: option(updateListNode('s))
}
/* Update rect that will be listen on the node
   that needs these rects to be drawn below. */
and updateRect('s) = {
  /* Rectangle mutable for resize,
     maybe this is not sufficient if nodes
     will be responsively rearranged */
  rect: Geom2d.Rect.t,
  /* Rect with scissor coords */
  scisRect: Geom2d.Rect.t,
  /* Node that may draw this rect, it will
     be checked for hidden and update state
     when setting active rects */
  rNode: updateListNode('s),
  /* List of stencils that are among child nodes,
     they will have active state determined by
     whether nodes are visible */
  stencils: list(updateStencil),
  /* Whether this rect covers the node it is drawing
     for. In which case, when node is visible, it is suffucient
     to stop at the current position in the list. */
  covers: bool,
  /* Whether the child requiring this rect needs to be drawn,
     also includes check whether this node is to be drawn */
  mutable active: bool
}
/* It would be nice to directly update these on resize etc */
and stencilBuffers = {
  mutable stencilVertices: Gpu.VertexBuffer.inited,
  mutable stencilIndices: Gpu.IndexBuffer.inited
}
and drawListItem('s) =
  | DrawNode(node('s))
  | SetRect(Geom2d.Rect.t)
  | ClearRect
  | DrawStencil(stencilBuffers)
  | ClearStencil
  | BindDrawTexture(node('s))
  | UnBindDrawTexture;

let quadVertices = Gpu.VertexBuffer.makeQuad();

let quadIndices = Gpu.IndexBuffer.makeQuad();

let makeLayout =
    (
      ~size=Dimensions(Scale(1.0), Scale(1.0)),
      ~padding=?,
      ~margin=?,
      ~spacing=?,
      ~childLayout=Horizontal,
      ~hAlign=AlignCenter,
      ~vAlign=AlignTop,
      ()
    )
    : layout => {
  size,
  padding,
  margin,
  childLayout,
  spacing,
  hAlign,
  vAlign
};

module SceneVO = {
  let make = (vertexBuffer, indexBuffer) => {
    nodes: [],
    vertexBuffer,
    indexBuffer
  };
};

type drawTo =
  | Framebuffer
  | TextureRGBA
  | TextureRGBADim(int)
  | TextureRGB
  | TextureRGBDim(int)
  | TextureGreyscale /* This is not usable, havent gotten greyscale to work yet, errors on not renderable */
  | TextureItem(Gpu.Texture.t);

module SceneTex = {
  let node = (~offsetX=Scale(0.0), ~offsetY=Scale(0.0), node) => {
    let texture =
      switch node.drawToTexture {
      | Some(texture) => texture
      | None => failwith("Provided node does not draw to texture")
      };
    {
      nodes: [],
      texture,
      texNode: Some(node.id),
      uniformMat: Some(UniformMat3f(ref(Data.Mat3.id()))),
      offsetX,
      offsetY
    };
  };
  let tex = (~offsetX=Scale(0.0), ~offsetY=Scale(0.0), texture) => {
    nodes: [],
    texture,
    texNode: None,
    uniformMat: None,
    offsetX,
    offsetY
  };
};

let makeProgram =
    (
      ~vertShader,
      ~fragShader,
      ~defaultUniforms=[],
      ~requiredUniforms=[],
      ~defaultTextures=[],
      ~requiredTextures=[],
      ~attribs,
      ~vo=?,
      ~layoutUniform=true,
      ~pixelSizeUniform=false,
      ~elapsedUniform=false,
      ~texTransUniform=false,
      ()
    )
    : sceneProgram => {
  /* Uniforms */
  let uniformList =
    List.append(
      List.map(((key, _uniform)) => key, defaultUniforms),
      List.map(((key, _uniform)) => key, requiredUniforms)
    );
  let defaultUniforms = Util.listToTbl(defaultUniforms);
  let requiredUniforms = Util.listToTbl(requiredUniforms);
  /* Textures */
  let textureList =
    List.append(
      List.map(((key, _uniform)) => key, defaultTextures),
      List.map(((key, _uniform)) => key, requiredTextures)
    );
  let defaultTextures =
    Util.listToTbl(
      List.map(
        ((name, t)) => (name, (t, Gpu.ProgramTexture.make(name, t.texture))),
        defaultTextures
      )
    );
  let requiredTextures = Util.listToTbl(requiredTextures);
  {
    inited: None,
    vertShader,
    fragShader,
    attribs,
    vo,
    uniformList,
    defaultUniforms,
    requiredUniforms,
    textureList,
    defaultTextures,
    requiredTextures,
    layoutUniform,
    pixelSizeUniform,
    elapsedUniform,
    texTransUniform
  };
};

let defaultVo = () => {
  nodes: [],
  vertexBuffer: quadVertices,
  indexBuffer: Some(quadIndices)
};

let makeNode =
    (
      ~key=?,
      ~cls=?,
      ~vertShader=?,
      ~fragShader=?,
      ~program: option(sceneProgram)=?,
      ~update=?,
      ~children=[],
      ~textures: list((string, sceneTexture))=[],
      ~attribs=?,
      ~vo: option(sceneVertexObject)=?,
      ~uniforms: list((string, sceneUniform))=[],
      ~layoutUniform=true,
      ~pixelSizeUniform=false,
      ~elapsedUniform=false,
      ~size=Dimensions(Scale(1.0), Scale(1.0)),
      ~padding=?,
      ~margin=?,
      ~childLayout=Horizontal,
      ~spacing=?,
      ~hAlign=AlignCenter,
      ~vAlign=AlignTop,
      ~selfDraw=true,
      ~loading=false,
      ~transparent=false,
      ~blendFactor=BlendAlpha,
      ~partialDraw=false,
      ~hidden=false,
      ~deps=[],
      ~drawTo=Framebuffer,
      ~texTransUniform=false,
      ~clearOnDraw=false,
      ()
    ) => {
  let textureList = List.map(((key, _texture)) => key, textures);
  let textures = Util.listToTbl(textures);
  let texMats =
    switch program {
    | None =>
      let texMats = Hashtbl.create(Hashtbl.length(textures));
      Hashtbl.iter(
        (key, tex) =>
          switch tex.uniformMat {
          | Some(mat) => Hashtbl.add(texMats, key, (tex, mat))
          | None => ()
          },
        textures
      );
      texMats;
    | Some(p) =>
      let texMats = Hashtbl.create(List.length(p.textureList));
      List.iter(
        key =>
          if (Hashtbl.mem(textures, key)) {
            let tex = Hashtbl.find(textures, key);
            switch tex.uniformMat {
            | Some(mat) => Hashtbl.add(texMats, key, (tex, mat))
            | None => ()
            };
          } else {
            /* Should be on default textures */
            let (dtex, _) = Hashtbl.find(p.defaultTextures, key);
            switch dtex.uniformMat {
            | Some(_) =>
              Hashtbl.add(
                texMats,
                key,
                (dtex, Gpu.UniformMat3f(ref(Data.Mat3.id())))
              )
            | None => ()
            };
          },
        p.textureList
      );
      texMats;
    };
  /* Provided uniform list */
  let uniformList = List.map(((key, _uniform)) => key, uniforms);
  let uniforms = Util.listToTbl(uniforms);
  /* If program, ensure required textures and uniforms are passed,
     and that all passed textures and uniforms are supported */
  switch program {
  | None => ()
  | Some(p) =>
    List.iter(
      key =>
        if (! Hashtbl.mem(p.requiredUniforms, key)
            && ! Hashtbl.mem(p.defaultUniforms, key)) {
          failwith("Uniform with key not supported by program: " ++ key);
        },
      uniformList
    );
    Hashtbl.iter(
      (key, _) =>
        if (! Hashtbl.mem(uniforms, key)) {
          failwith("Required uniform: " ++ key ++ " not defined");
        },
      p.requiredUniforms
    );
    List.iter(
      key =>
        if (! Hashtbl.mem(p.requiredTextures, key)
            && ! Hashtbl.mem(p.defaultTextures, key)) {
          failwith("Texture with key not supported by program: " ++ key);
        },
      textureList
    );
    Hashtbl.iter(
      (key, _) =>
        if (! Hashtbl.mem(textures, key)) {
          failwith("Required texture: " ++ key ++ " not defined");
        },
      p.requiredTextures
    );
  };
  /* System uniforms */
  let (progLayoutU, progPixelSizeU, progElapsedU, progTexTransU) =
    switch program {
    | None => (false, false, false, false)
    | Some(program) => (
        program.layoutUniform,
        program.pixelSizeUniform,
        program.elapsedUniform,
        program.texTransUniform
      )
    };
  /* Could also allow stringly typed uniforms to create this,
     There is a slight advantage to not search map
     for each node in layout calculation, so nice to
     have datatype */
  let layoutUniform =
    if (layoutUniform || progLayoutU) {
      Some(Gpu.UniformMat3f(ref(Data.Mat3.id())));
    } else {
      None;
    };
  let pixelSizeUniform =
    if (pixelSizeUniform || progPixelSizeU) {
      Some(Gpu.UniformVec2f(ref(Data.Vec2.zeros())));
    } else {
      None;
    };
  let elapsedUniform =
    if (elapsedUniform || progElapsedU) {
      Some(Gpu.UniformFloat(ref(0.0)));
    } else {
      None;
    };
  let vo =
    switch vo {
    | Some(vo) => Some(vo)
    | None =>
      switch program {
      | Some(p) =>
        switch p.vo {
        | Some(_) => None /* Can use vo from program */
        | None => Some(defaultVo())
        }
      | None => Some(defaultVo())
      }
    };
  let attribs =
    switch (program, attribs, vo) {
    | (None, None, Some(vo)) =>
      Some(Gpu.VertexBuffer.createAttribs(vo.vertexBuffer))
    | (None, Some(_) as attribs, _) => attribs
    | (Some(_), None, _) => None
    | (None, None, None) => failwith("Attribs, program or vo is required")
    | (Some(_), Some(_), _) =>
      failwith("Attribs not usable when program is provided")
    };
  let layout = {size, padding, margin, childLayout, spacing, hAlign, vAlign};
  /* Easy way to create texture to draw to, this can
     be used in tandem with ~texNodes.
     Todo: pool of textures to draw to? This would
     require some texture refs to be changed
     when there is no more room and a new
     texture needs to be used
     Or possibly the size of the texture could be changed */
  let drawToTexture =
    switch drawTo {
    | Framebuffer => None
    | TextureRGB =>
      let texture = Gpu.Texture.makeEmptyRgb();
      Some(texture);
    | TextureRGBDim(dim) =>
      let texture = Gpu.Texture.makeEmptyRgb(~width=dim, ~height=dim, ());
      Some(texture);
    | TextureRGBA =>
      let texture = Gpu.Texture.makeEmptyRgba();
      Some(texture);
    | TextureRGBADim(dim) =>
      let texture = Gpu.Texture.makeEmptyRgba(~width=dim, ~height=dim, ());
      Some(texture);
    | TextureGreyscale =>
      let texture = Gpu.Texture.makeEmptyGreyscale();
      Some(texture);
    | TextureItem(texture) => Some(texture)
    };
  /* Texture transformation could either
     come with layout uniform, or in a separate
     uniform for example if the shader wants
     to do lighting based on layout coords
     Todo: allow non drawTo nodes that might
     be children of drawTo nodes to get this
     variable */
  let texTransUniform =
    switch (drawTo, texTransUniform, progTexTransU) {
    | (Framebuffer, _, _) => None
    | (_, false, false) => None
    | (_, true, _)
    | (_, _, true) => Some(Gpu.UniformMat3f(ref(Data.Mat3.id())))
    };
  {
    key,
    cls,
    id: nextNodeId(),
    onUpdate: update,
    drawState: None,
    layout,
    cLayout: {
      pWidth: 0.0,
      pHeight: 0.0,
      inWidth: 0.0,
      inHeight: 0.0,
      marginX1: 0.0,
      marginX2: 0.0,
      marginY1: 0.0,
      marginY2: 0.0,
      pXOffset: 0.0,
      pYOffset: 0.0
    },
    rect: Geom2d.Rect.zeros(),
    outRect: Geom2d.Rect.zeros(),
    scissorRect: Geom2d.Rect.zeros(),
    screenRect: Geom2d.Rect.zeros(),
    selfDraw,
    loading,
    transparent,
    blendFactor,
    partialDraw,
    hidden,
    children,
    deps,
    vertShader,
    fragShader,
    program,
    textureList,
    textures,
    texMats,
    vo,
    attribs,
    uniformList,
    uniforms,
    layoutUniform,
    pixelSizeUniform,
    elapsedUniform,
    drawToTexture,
    clearOnDraw,
    texTransUniform,
    parent: None,
    scene: None
  };
};

let setNodeParentsSceneKeyCls = scene => {
  let rec loop = (node, parent) => {
    node.parent = parent;
    node.scene = Some(scene);
    switch node.key {
    | None => ()
    | Some(key) =>
      if (Hashtbl.mem(scene.nodesByKey, key)) {
        failwith("Node with key already exist: " ++ key);
      } else {
        Hashtbl.add(scene.nodesByKey, key, node);
      }
    };
    switch node.cls {
    | None => ()
    | Some(cls) =>
      if (Hashtbl.mem(scene.nodesByCls, cls)) {
        let current = Hashtbl.find(scene.nodesByCls, cls);
        Hashtbl.replace(scene.nodesByCls, cls, [node, ...current]);
      } else {
        Hashtbl.add(scene.nodesByCls, cls, [node]);
      }
    };
    List.iter(dep => loop(dep, Some(node)), node.deps);
    List.iter(child => loop(child, Some(node)), node.children);
  };
  loop(scene.root, None);
};

/* Can also consider using depth buffer
   and draw from closest down parents when
   drawing layout things */
/* Performance is important for this function,
   so will look for more opportunities to cache etc.
   Possibly we could use an array sized for all
   nodes in the three and jump for children
   and deps.
   Use new collections from bucklescript. */
let buildUpdateTree = (scene, root) => {
  /* Also adds nodes to uniforms, vertexobjects and textures objects */
  let rec loop = (node, parent, isDep, isDepRoot, prevStacked) => {
    /* Feels maybe better to keep lists on scene instead of on these objects */
    Hashtbl.iter(
      (_key, uniform) => uniform.nodes = [node.id, ...uniform.nodes],
      node.uniforms
    );
    switch node.vo {
    | Some(vo) => vo.nodes = [node.id, ...vo.nodes]
    | None =>
      switch node.program {
      | Some(p) =>
        switch p.vo {
        | Some(vo) => vo.nodes = [node.id, ...vo.nodes]
        | None => failwith("Could not find vo")
        }
      | None => failwith("Could not find vo")
      }
    };
    Hashtbl.iter(
      (_key, tex: sceneTexture) => tex.nodes = [node.id, ...tex.nodes],
      node.textures
    );
    /* Check for hidden */
    if (node.hidden) {
      scene.hiddenNodes = [node.id, ...scene.hiddenNodes];
    };
    let stencil =
      if (node.transparent || node.partialDraw) {
        None;
      } else {
        /* Translate to screen coordinates */
        let updStencil = {rect: node.screenRect, active: false};
        /* There may not be much point to
           add stencil when isDepRoot */
        if (! isDepRoot) {
          /* Add reference to parent */
          switch parent {
          | Some(parent) => parent.stencils = [updStencil, ...parent.stencils]
          | None => ()
          };
        };
        Some(updStencil);
      };
    /* Possibly, this could be created after
       collecting deps and children, in which
       case parent field would need to be
       filled in. Not sure if the difference
       is very big, see if there is some use
       to getting the parent handy first */
    let updateNode = {
      update: false,
      childUpdate: false,
      isDep,
      isDepRoot,
      updNode: node,
      rect: None,
      stencils: [],
      stencil,
      childRects: [],
      parentRects: [],
      updDeps: [],
      updChildren: [],
      parent,
      prevStacked,
      nextStacked: []
    };
    /* Add to indexes for node id/number and flag */
    scene.updateNodes[node.id] = Some(updateNode);
    updateNode.updDeps =
      List.map(dep => loop(dep, Some(updateNode), true, true, []), node.deps);
    switch node.layout.childLayout {
    | Stacked =>
      /* Loop stacked children, and pass on accumulated children as prevStacked */
      let updChildren =
        List.fold_left(
          (updChildren, stackedChild) => {
            let updChild =
              loop(stackedChild, Some(updateNode), isDep, false, updChildren);
            [updChild, ...updChildren];
          },
          [],
          node.children
        );
      /* Set nextStacked, updChildren is in reverse order from original child order */
      let _ =
        List.fold_left(
          (nextStacked, updChild) => {
            updChild.nextStacked = nextStacked;
            [updChild, ...nextStacked];
          },
          [],
          updChildren
        );
      updateNode.updChildren = List.rev(updChildren);
    | _ =>
      updateNode.updChildren =
        List.map(
          child => loop(child, Some(updateNode), isDep, false, []),
          node.children
        )
    };
    /* All children have added their stencils,
       propagate them to prevStacked and parent */
    let rec addStencilsToNodeAndChildren = updNode => {
      /* Add to updNode */
      List.iter(
        updStencil => updNode.stencils = [updStencil, ...updNode.stencils],
        updateNode.stencils
      );
      /* And updNodes children */
      List.iter(
        updNodeChild => addStencilsToNodeAndChildren(updNodeChild),
        updNode.updChildren
      );
    };
    List.iter(
      prevStacked => addStencilsToNodeAndChildren(prevStacked),
      prevStacked
    );
    if (! updateNode.isDepRoot) {
      switch updateNode.parent {
      | Some(parent) =>
        List.iter(
          updStencil => parent.stencils = [updStencil, ...parent.stencils],
          updateNode.stencils
        )
      | None => ()
      };
    };
    updateNode;
  };
  scene.updateNodes = Array.make(currNodeId^, None);
  let tree = loop(root, None, false, false, []);
  /* Sort added hidden nodes */
  scene.hiddenNodes = List.sort((a, b) => a > b ? 1 : (-1), scene.hiddenNodes);
  tree;
};

let make = (canvas, state, root, ~drawListDebug=false, ~initQueued=true, ()) => {
  let drawListsDebug =
    switch drawListDebug {
    | false => None
    | true => Some({draw: DrawListDebug.make(canvas)})
    };
  let scene = {
    state,
    canvas,
    queuedUpdates: [],
    nodesByKey: Hashtbl.create(30),
    nodesByCls: Hashtbl.create(30),
    root,
    inited: false,
    drawLists: Hashtbl.create(30),
    drawListsDebug,
    initedLists: Hashtbl.create(30),
    initedDeps: Hashtbl.create(10),
    loadingNodes: [],
    anims: [],
    fbuffer: Hashtbl.create(1),
    stencilDraw: StencilDraw.make(canvas),
    updateNodes: [||],
    updateRoot: None,
    hiddenNodes: [],
    queuedDrawStates: [],
    queuedDeps: [],
    initQueued
  };
  setNodeParentsSceneKeyCls(scene);
  scene.updateRoot = Some(buildUpdateTree(scene, scene.root));
  scene;
};

let nodeDescr = node =>
  switch node.key {
  | Some(key) => key ++ " id:" ++ string_of_int(node.id)
  | None =>
    switch node.cls {
    | Some(cls) => "cls: " ++ cls ++ " id:" ++ string_of_int(node.id)
    | None => "id:" ++ string_of_int(node.id)
    }
  };

let nodeIdDescr = (scene, nodeId) =>
  switch scene.updateNodes[nodeId] {
  | Some(n) => nodeDescr(n.updNode)
  | None => failwith("Could not find node with id: " ++ string_of_int(nodeId))
  };

let createDrawStateUniforms =
    (
      texMats,
      elapsedUniform,
      pixelSizeUniform,
      layoutUniform,
      texTransUniform,
      customUniforms
    ) => {
  /* Texture uniforms */
  let uniforms =
    List.fold_left(
      (uniforms, (name, uniform)) => [
        Gpu.Uniform.make(name ++ "Mat", uniform),
        ...uniforms
      ],
      [],
      texMats
    );
  /* Elapsed uniform */
  let uniforms =
    switch elapsedUniform {
    | Some(elapsedUniform) => [
        Gpu.Uniform.make("elapsedScene", elapsedUniform),
        ...uniforms
      ]
    | None => uniforms
    };
  /* PixelSize uniform */
  let uniforms =
    switch pixelSizeUniform {
    | Some(pixelSizeUniform) => [
        Gpu.Uniform.make("pixelSize", pixelSizeUniform),
        ...uniforms
      ]
    | None => uniforms
    };
  /* Layout uniform */
  let uniforms =
    switch layoutUniform {
    | Some(layoutUniform) => [
        Gpu.Uniform.make("layout", layoutUniform),
        ...uniforms
      ]
    | None => uniforms
    };
  /* TexTrans uniform */
  let uniforms =
    switch texTransUniform {
    | Some(texTransUniform) => [
        Gpu.Uniform.make("texTrans", texTransUniform),
        ...uniforms
      ]
    | None => uniforms
    };
  List.append(uniforms, customUniforms);
};

let initSceneProgram = (scene, sceneP: sceneProgram) =>
  switch sceneP.inited {
  | Some(inited) => inited
  | None =>
    let context = scene.canvas.context;
    let gpuState = scene.canvas.gpuState;
    /* Custom uniforms */
    let uniforms =
      List.map(
        key =>
          if (Hashtbl.mem(sceneP.defaultUniforms, key)) {
            let sceneU = Hashtbl.find(sceneP.defaultUniforms, key);
            Gpu.Uniform.make(key, sceneU.uniform);
          } else if (Hashtbl.mem(sceneP.requiredUniforms, key)) {
            /* Locs from these will be weaved into uniforms
               provided by nodes */
            /* Slightly uneccesary to wrap this in Uniform.t */
            let reqType = Hashtbl.find(sceneP.requiredUniforms, key);
            Gpu.Uniform.make(key, Gpu.Uniform.makeValRefFromType(reqType));
          } else {
            failwith("Could not find uniform from list key");
          },
        sceneP.uniformList
      );
    let program =
      Gpu.Program.make(
        sceneP.vertShader,
        sceneP.fragShader,
        sceneP.attribs,
        uniforms
      );
    let iProgram =
      switch (Gpu.Program.init(program, context)) {
      | Some(iProgram) => iProgram
      | None => failwith("Could not init program")
      };
    /* Texture uniform locations of all, both default and required, textures */
    let iTextureU = Hashtbl.create(List.length(sceneP.textureList));
    /* Initialized default textures */
    let iTexture = Hashtbl.create(Hashtbl.length(sceneP.defaultTextures));
    /* Texture mat uniforms
       (has some redundant space since some dont have mat
       Could maybe collect list, then create hashtbl,
       not sure if it's worth it) */
    let iTextureMat = Hashtbl.create(List.length(sceneP.textureList));
    Hashtbl.iter(
      (key, (sceneTex, progTex)) => {
        let iTex =
          Gpu.ProgramTexture.init(progTex, context, iProgram.programRef);
        Hashtbl.add(iTexture, key, iTex);
        Hashtbl.add(iTextureU, key, iTex.uniformRef);
        switch sceneTex.uniformMat {
        | None => ()
        | Some(_) =>
          Hashtbl.add(
            iTextureMat,
            key,
            Gpu.Uniform.initLoc(context, iProgram.programRef, key ++ "Mat")
          )
        };
      },
      sceneP.defaultTextures
    );
    Hashtbl.iter(
      (key, requireMat) => {
        Hashtbl.add(
          iTextureU,
          key,
          Gpu.ProgramTexture.initRef(key, context, iProgram.programRef)
        );
        if (requireMat) {
          Hashtbl.add(
            iTextureMat,
            key,
            Gpu.Uniform.initLoc(context, iProgram.programRef, key ++ "Mat")
          );
        };
      },
      sceneP.requiredTextures
    );
    let layoutUniform =
      if (sceneP.layoutUniform) {
        Some(Gpu.Uniform.initLoc(context, iProgram.programRef, "layout"));
      } else {
        None;
      };
    let pixelSizeUniform =
      if (sceneP.layoutUniform) {
        Some(Gpu.Uniform.initLoc(context, iProgram.programRef, "pixelSize"));
      } else {
        None;
      };
    let elapsedUniform =
      if (sceneP.layoutUniform) {
        Some(Gpu.Uniform.initLoc(context, iProgram.programRef, "elapsed"));
      } else {
        None;
      };
    let texTransUniform =
      if (sceneP.layoutUniform) {
        Some(Gpu.Uniform.initLoc(context, iProgram.programRef, "texTrans"));
      } else {
        None;
      };
    let iVo =
      switch sceneP.vo {
      | Some(vo) =>
        let iVertices =
          Gpu.VertexBuffer.init(vo.vertexBuffer, context, gpuState);
        let iIndices =
          switch vo.indexBuffer {
          | Some(indices) =>
            Some(Gpu.IndexBuffer.init(indices, context, gpuState))
          | None => None
          };
        Some((iVertices, iIndices));
      | None => None
      };
    {
      iProgram,
      iTexture,
      iTextureU,
      iTextureMat,
      iVo,
      layoutUniform,
      pixelSizeUniform,
      elapsedUniform,
      texTransUniform
    };
  };

/* Creates a drawState for a node which has a sceneProgram,
   the drawState contains uniform refs from sceneProgram,
   may have some resources, textures and buffers, from
   sceneProgram, and some from the node. This
   is implemented by weaving some resources from
   sceneProgramInited into uniforms etc here */
let createProgramDrawState = (scene, node, program: sceneProgram) => {
  let pInited = initSceneProgram(scene, program);
  let context = scene.canvas.context;
  let gpuState = scene.canvas.gpuState;
  /* List of regular/custom uniforms */
  /* todo: decouple program from uniforms? */
  let (_, uniforms) =
    List.fold_left(
      ((iUniforms: list(Gpu.Uniform.inited), uniforms), key) =>
        switch iUniforms {
        | [iUniform, ...restIUniforms] =>
          if (Hashtbl.mem(node.uniforms, key)) {
            let sUniform = Hashtbl.find(node.uniforms, key);
            (
              restIUniforms,
              [
                Gpu.Uniform.initedFromLoc(
                  Gpu.Uniform.getUniformGlType(sUniform.uniform),
                  sUniform.uniform,
                  iUniform.loc
                ),
                ...uniforms
              ]
            );
          } else if (Hashtbl.mem(program.defaultUniforms, key)) {
            (restIUniforms, [iUniform, ...uniforms]);
          } else {
            failwith("Could not find uniform by name: " ++ key);
          }
        | [] =>
          failwith(
            "Scene program uniform mismatch between inited program and node creating drawstate"
          )
        },
      (pInited.iProgram.uniforms, []),
      program.uniformList
    );
  let uniforms =
    switch (node.texTransUniform, pInited.texTransUniform) {
    | (Some(u), Some(l)) => [
        Gpu.Uniform.initedFromLoc(Gpu.GlType.Mat3f, u, l),
        ...uniforms
      ]
    | (_, _) => uniforms
    };
  let uniforms =
    switch (node.elapsedUniform, pInited.elapsedUniform) {
    | (Some(u), Some(l)) => [
        Gpu.Uniform.initedFromLoc(Gpu.GlType.Float, u, l),
        ...uniforms
      ]
    | (_, _) => uniforms
    };
  let uniforms =
    switch (node.pixelSizeUniform, pInited.pixelSizeUniform) {
    | (Some(u), Some(l)) => [
        Gpu.Uniform.initedFromLoc(Gpu.GlType.Vec2f, u, l),
        ...uniforms
      ]
    | (_, _) => uniforms
    };
  let uniforms =
    switch (node.layoutUniform, pInited.layoutUniform) {
    | (Some(u), Some(l)) => [
        Gpu.Uniform.initedFromLoc(Gpu.GlType.Mat3f, u, l),
        ...uniforms
      ]
    | (_, _) => uniforms
    };
  /* Texture mats */
  let uniforms =
    List.fold_left(
      (uniforms, name) =>
        if (Hashtbl.mem(node.texMats, name)) {
          let (_, matUniform) = Hashtbl.find(node.texMats, name);
          [
            Gpu.Uniform.initedFromLoc(
              Gpu.GlType.Mat3f,
              matUniform,
              Hashtbl.find(pInited.iTextureMat, name)
            ),
            ...uniforms
          ];
        } else {
          uniforms;
        },
      uniforms,
      program.textureList
    );
  let textures =
    List.map(
      name =>
        if (Hashtbl.mem(node.textures, name)) {
          let tex = Hashtbl.find(node.textures, name);
          Gpu.ProgramTexture.initFromRef(
            Gpu.ProgramTexture.make(name, tex.texture),
            context,
            Hashtbl.find(pInited.iTextureU, name)
          );
        } else if (Hashtbl.mem(pInited.iTexture, name)) {
          Hashtbl.find(pInited.iTexture, name);
        } else {
          failwith("Could not find texture");
        },
      program.textureList
    );
  let (vertexBuffer, indexBuffer) =
    switch node.vo {
    | Some(vo) => (
        Gpu.VertexBuffer.init(vo.vertexBuffer, context, gpuState),
        switch vo.indexBuffer {
        | Some(i) => Some(Gpu.IndexBuffer.init(i, context, gpuState))
        | None => None
        }
      )
    | None =>
      switch pInited.iVo {
      | Some(iVo) => iVo
      | None => failwith("Neither node nor program has vertexobject")
      }
    };
  node.drawState =
    Some(
      Gpu.DrawState.fromInited(
        Gpu.Program.initedWithUniforms(pInited.iProgram, List.rev(uniforms)),
        vertexBuffer,
        indexBuffer,
        Array.of_list(textures)
      )
    );
};

let createNodeDrawState = (scene, node) => {
  let gpuState = scene.canvas.gpuState;
  let texNames =
    List.fold_left(
      (texNames, name) => {
        let nodeTex = Hashtbl.find(node.textures, name);
        switch nodeTex.uniformMat {
        | Some(u) => [(name, u), ...texNames]
        | None => texNames
        };
      },
      [],
      node.textureList
    );
  let nodeUniforms =
    List.map(
      key => {
        let sUniform = Hashtbl.find(node.uniforms, key);
        Gpu.Uniform.make(key, sUniform.uniform);
      },
      node.uniformList
    );
  let uniforms =
    createDrawStateUniforms(
      texNames,
      node.elapsedUniform,
      node.pixelSizeUniform,
      node.layoutUniform,
      node.texTransUniform,
      nodeUniforms
    );
  let textures =
    List.map(
      key => {
        let nodeTex = Hashtbl.find(node.textures, key);
        Gpu.ProgramTexture.make(key, nodeTex.texture);
      },
      node.textureList
    );
  let vertShader =
    switch node.vertShader {
    | Some(vertShader) => vertShader
    | None => failwith("Vertex shader not found on: " ++ nodeDescr(node))
    };
  let fragShader =
    switch node.fragShader {
    | Some(fragShader) => fragShader
    | None => failwith("Fragment shader not found on: " ++ nodeDescr(node))
    };
  let (vBuffer, iBuffer) =
    switch node.vo {
    | Some(vo) => (vo.vertexBuffer, vo.indexBuffer)
    | None => failwith("Could not find vo")
    };
  let attribs =
    switch node.attribs {
    | Some(attribs) => attribs
    | None => failwith("Program or attribs is required")
    };
  node.drawState =
    Some(
      Gpu.DrawState.init(
        scene.canvas.context,
        Gpu.Program.make(vertShader, fragShader, attribs, uniforms),
        vBuffer,
        iBuffer,
        textures,
        gpuState
      )
    );
};

/* Creates a drawstate for a node, possibly using it's
   "sceneProgram", which are reusable programs with
   possibly default data. This drawstate is data
   needed to perform a draw like uniforms, vertices,
   textures */
let createDrawState = (scene, node) =>
  if (node.selfDraw) {
    switch node.program {
    | None => createNodeDrawState(scene, node)
    | Some(p) => createProgramDrawState(scene, node, p)
    };
  };

/* Queues uncreated drawstates so they are
   created between frames forward from then.
   Consider heuristics for what is
   likely to be needed next. Probably the lowest
   level nodes are likely to be needed */
let queueDrawStates = scene => {
  let rec loop = (node, list) => {
    let list =
      List.fold_left((list, child) => loop(child, list), list, node.children);
    let list = List.fold_left((list, dep) => loop(dep, list), list, node.deps);
    if (node.selfDraw && node.drawState == None) {
      [node, ...list];
    } else {
      list;
    };
  };
  scene.queuedDrawStates = loop(scene.root, []);
};

/* Add nodes that needs update, and it should be
   done on next render/frame */
let queueUpdates = (scene, nodes) =>
  scene.queuedUpdates =
    List.fold_left(
      (updates, id) => [id, ...updates],
      scene.queuedUpdates,
      nodes
    );

/* Creates animation record */
let makeAnim =
    (
      onFrame,
      setLast,
      duration,
      ~key=?,
      ~next=?,
      ~frameInterval=1,
      ~onDone=?,
      ()
    ) => {
  animKey: key,
  onFrame,
  setLast,
  onDone,
  duration,
  elapsed: 0.0,
  frameInterval,
  numFrames: 0,
  next
};

/* Removes any active anims with given key from anim list */
let clearAnim = (scene, animKey) =>
  scene.anims =
    List.filter(
      a =>
        switch a.animKey {
        | Some(key) => key != animKey
        | None => true
        },
      scene.anims
    );

let rec setChildToUpdate = updNode =>
  if (! updNode.updNode.hidden) {
    updNode.update = true;
    List.iter(
      updChild =>
        if (! updChild.update) {
          setChildToUpdate(updChild);
        },
      updNode.updChildren
    );
  };

let rec setDepParentToUpdate = updNode =>
  if (! updNode.updNode.hidden) {
    /* todo: more fine grained update
       when coming from deps. Currently
       just turning on update
       There should be a rect propagated up
       to node with this dependency, then
       to it's children */
    updNode.update = true;
    /* This might be uneccesary if we do more
       fine grained update */
    List.iter(
      updChild =>
        if (! updChild.update) {
          setChildToUpdate(updChild);
        },
      updNode.updChildren
    );
    switch updNode.parent {
    | Some(parent) =>
      if (parent.isDep) {
        if (! parent.update) {
          setDepParentToUpdate(parent);
        };
      } else if
        /* Propagate to regular update of node that
           has this as dependency */
        (! parent.update) {
        setToUpdate(parent);
      }
    | None => ()
    };
  }
and setToUpdate = updNode =>
  if (! updNode.updNode.hidden && ! updNode.update) {
    updNode.update = true;
    /* If this is stacked layout, set
       previous children to update
       todo: Could possibly be rect/stencil */
    List.iter(
      prevStacked =>
        if (! prevStacked.update) {
          setToUpdate(prevStacked);
        },
      updNode.prevStacked
    );
    /* Set children to update */
    List.iter(
      updChild =>
        if (! updChild.update) {
          setChildToUpdate(updChild);
        },
      updNode.updChildren
    );
    /* Then next stacked if this is in the middle
       of a stacked layout */
    List.iter(
      nextStacked =>
        if (! nextStacked.update) {
          setToUpdate(nextStacked);
        },
      updNode.nextStacked
    );
    /* If this is a dep, set dependant parent also to update */
    if (updNode.isDep) {
      switch updNode.parent {
      | Some(parent) =>
        if (! parent.update) {
          if (parent.isDep) {
            setDepParentToUpdate(parent);
          } else {
            setToUpdate(parent);
          };
        }
      | None => failwith("Dependency without parent")
      };
    };
    /* Mark upwards in the tree that a child
       needs update */
    let rec markChildUpdate = updNode => {
      updNode.childUpdate = true;
      switch updNode.parent {
      | Some(parent) when parent.childUpdate == false =>
        markChildUpdate(parent)
      | _ => ()
      };
    };
    switch updNode.parent {
    | Some(parent) when parent.childUpdate == false => markChildUpdate(parent)
    | _ => ()
    };
  };

/* Traverses parents to ensure nodes that
   are set to update, and are transparent
   or partialDraw, are covered by a parent */
let actRectUntilCovered = updNode => {
  /* Repeating some checks a few places here
     percievely in the interest of a little
     performance as well as slightly
     different cases */
  let actStencil = updNode =>
    /* Stencil should be present
       when node is not transparent
       or partialDraw */
    /* Todo:(!) If we had rendered from
       child upwards when not needing
       to draw transparent nodes,
       the stencil mask could
       come from those renders, and the
       stencil mask could be increased as
       more is drawn, should be a nice
       optimization */
    switch updNode.stencil {
    | Some(stencil) => stencil.active = true
    | None => ()
    };
  /* This creates new updRects for unexplored
     parents, sets their active status and return
     a list of newly created updateRects until
     a covering node is found */
  let rec loopNewRects = (parNode: updateListNode('s)) => {
    /* First process any prevStacked */
    let next =
      switch parNode.prevStacked {
      | [] => parNode.parent
      | [prev, ..._rest] => Some(prev)
      };
    switch next {
    | Some(next) =>
      if (! next.updNode.selfDraw) {
        /* Skipping layout nodes now,
           possibly there could be use of
           having rects there if they propagate
           to their children */
        loopNewRects(
          next
        );
      } else {
        let covers =
          ! (next.updNode.transparent || next.updNode.partialDraw)
          && Geom2d.Rect.contains(next.updNode.rect, updNode.updNode.rect);
        let updRect = {
          rect: updNode.updNode.rect,
          scisRect: updNode.updNode.scissorRect,
          rNode: next,
          stencils: [],
          covers,
          active: ! (next.updNode.hidden || next.update)
        };
        next.childRects = [updRect, ...next.childRects];
        /* For normal childs they are hidden when parent
           are hidden, but stacked layouts are
           rearranged so a parent may be hidden while
           a child is not */
        if (next.updNode.hidden) {
          [
            /* Pass on rect for possible later use, but it will not be active */
            updRect,
            ...loopNewRects(updRect.rNode)
          ];
        } else if (next.update) {
          if (updRect.covers) {
            [
              /* Update and covers, done */
              updRect
            ];
          } else {
            actStencil(updRect.rNode);
            [updRect, ...loopNewRects(updRect.rNode)];
          };
        } else if (updRect.covers) {
          [updRect];
        } else {
          actStencil(updRect.rNode);
          [updRect, ...loopNewRects(updRect.rNode)];
        };
      }
    | None =>
      failwith(
        "Could not find covering parent for: "
        ++ nodeDescr(updNode.updNode)
        ++ ". Maybe add a background node?"
      )
    };
  };
  let rec loopRects = (rects: list(updateRect('s))) =>
    switch rects {
    | [updRect] =>
      /* Check if we need to continue
         or have found a covering node */
      if (updRect.rNode.updNode.hidden) {
        updNode.parentRects =
          List.append(updNode.parentRects, loopNewRects(updRect.rNode));
      } else if (updRect.rNode.update) {
        if (! updRect.covers) {
          actStencil(updRect.rNode);
          updNode.parentRects =
            List.append(updNode.parentRects, loopNewRects(updRect.rNode));
        };
      } else {
        updRect.active = true;
        if (! updRect.covers) {
          actStencil(updRect.rNode);
          updNode.parentRects =
            List.append(updNode.parentRects, loopNewRects(updRect.rNode));
        };
      }
    | [updRect, ...rest] =>
      if (updRect.rNode.updNode.hidden) {
        loopRects(rest);
      } else if (updRect.rNode.update) {
        if (! updRect.covers) {
          actStencil(updRect.rNode);
          loopRects(rest);
        };
      } else {
        updRect.active = true;
        if (! updRect.covers) {
          actStencil(updRect.rNode);
          loopRects(rest);
        };
      }
    | [] =>
      /* This should only be case for initial empty list,
         or actual empty list for rootish node */
      updNode.parentRects = loopNewRects(updNode)
    };
  loopRects(updNode.parentRects);
};

let rec setAdjecentStackedToUpdate = (updNode: option(updateListNode('s))) =>
  switch updNode {
  | Some(updNode) =>
    setAdjecentStackedToUpdate(updNode.parent);
    /* todo:(!) improve handling of adjecent stacked with stencil/rects */
    let numAdded =
      List.fold_left(
        (num, stacked) =>
          if (! stacked.updNode.hidden) {
            if (! stacked.update) {
              setToUpdate(stacked);
            };
            num + 1;
          } else {
            num;
          },
        0,
        List.append(updNode.prevStacked, updNode.nextStacked)
      );
    /* We also need to update parent if adjecent
       stack is updated (see todo) */
    if (numAdded > 0) {
      if (! updNode.updNode.hidden) {
        if (! updNode.update) {
          setToUpdate(updNode);
        };
      };
    };
  | None => ()
  };

/* UpdateNodes should be checked for visibility up front */
/* Todo: Optimization, draw equals programs after eachother
   when order and layout allows. Algorithm for this would
   have some cost, and needs to be checked */
let createDrawList = (scene, updateNodes, updRoot) => {
  let gpuState = scene.canvas.gpuState;
  let updRoot =
    switch scene.updateNodes[updRoot.id] {
    | Some(updRoot) => updRoot
    | None => failwith("Root update node not found")
    };
  /* Set nodes to update */
  let updNodes = [
    List.fold_left(
      (nodes, nodeId) =>
        switch scene.updateNodes[nodeId] {
        | Some(updNode) =>
          setToUpdate(updNode);
          [updNode, ...nodes];
        | None =>
          failwith(
            "Could not find update node with id: " ++ string_of_int(nodeId)
          )
        },
      [],
      updateNodes
    ),
    []
  ];
  List.iter(
    nodes =>
      List.iter(
        (updNode: updateListNode('s)) =>
          /* Nodes could be part of a stacked layout
             here or further up in the scene. Traverse
             node and parents and for now, simply flag
             update if finding stacked nodes. (could be stencil/rect) */
          setAdjecentStackedToUpdate(Some(updNode)),
        nodes
      ),
    updNodes
  );
  /* Assuming nodes that need update are flagged,
     do second pass on nodes to update
     to check for transparent nodes and
     ensure they are covered by a parent */
  let rec actRecLoop = (updNode: updateListNode('s)) =>
    if (updNode.update) {
      if (updNode.updNode.transparent || updNode.updNode.partialDraw) {
        actRectUntilCovered(updNode);
      };
      List.iter(child => actRecLoop(child), updNode.updChildren);
    } else if (updNode.childUpdate) {
      List.iter(child => actRecLoop(child), updNode.updChildren);
    };
  actRecLoop(updRoot);
  /* State of stencils and rect to determine
     when to clear them */
  let activeStencils = ref(None);
  let activeRect = ref(None);
  let equalsActiveStencils = (stencils: list(updateStencil)) => {
    let rec checkStencils =
            (
              stencils: list(updateStencil),
              activeStencils: list(updateStencil)
            ) =>
      switch (stencils, activeStencils) {
      | ([], []) => true
      | ([st1, ...rest1], [st2, ...rest2]) =>
        if (Geom2d.Rect.equals(st1.rect, st2.rect)) {
          false;
        } else {
          checkStencils(rest1, rest2);
        }
      | _ => false
      };
    switch activeStencils^ {
    | None => false
    | Some(activeStencils) => checkStencils(stencils, activeStencils)
    };
  };
  let equalsActiveRect = rect =>
    switch activeRect^ {
    | None => false
    | Some(activeRect) => Geom2d.Rect.equals(activeRect, rect)
    };
  /* Generate draw list
     Here we look at activated stencils and
     rects, and create a drawlist with instructions
     on how to draw those and the nodes themselves.
     The returned list will be reversed */
  let rec drawListLoop = (updNode, drawList, hidden) => {
    /* Propagating hidden variable. It is not ideal
       to do all else, but for now atleast.
       Need also to make sure whole tree is reset,
       So otherwise should ensure there are no hidden
       parents when setting a node to update.
       Possibly we might also have some logic
       to cancel hidden status in children */
    let hidden = hidden || updNode.updNode.hidden;
    /* Clean up childUpdate flag */
    if (updNode.childUpdate) {
      updNode.childUpdate = false;
    };
    /* First collect from dependencies */
    let drawList =
      List.fold_left(
        (drawList, dep) => drawListLoop(dep, drawList, hidden),
        drawList,
        updNode.updDeps
      );
    /* Check for selfDraw.
       Not sure where to branch on this,
       doing after deps now,
       especially if we render from child to parent
       when not transparent, it could make sense
       to update stencils */
    if (! updNode.updNode.selfDraw) {
      /* Allow draw texture also for nodes that don't self draw */
      let (drawList, drawToTexture) =
        if (updNode.update) {
          updNode.update = false;
          switch updNode.updNode.drawToTexture {
          | Some(_tex) => (
              [BindDrawTexture(updNode.updNode), ...drawList],
              true
            )
          | None => (drawList, false)
          };
        } else {
          (drawList, false);
        };
      /* Recurse to children if they have update or childUpdate flagged */
      let drawList =
        List.fold_left(
          (drawList, updChild) =>
            if (updChild.update) {
              drawListLoop(updChild, drawList, hidden);
            } else if (updChild.childUpdate) {
              drawListLoop(updChild, drawList, hidden);
            } else {
              drawList;
            },
          drawList,
          updNode.updChildren
        );
      let drawList =
        if (drawToTexture) {
          [UnBindDrawTexture, ...drawList];
        } else {
          drawList;
        };
      drawList;
    } else if (hidden) {
      updNode.update = false;
      /* Clean up active flag on rects */
      List.iter(
        (updRect: updateRect('s)) => updRect.active = false,
        updNode.childRects
      );
      /* Recurse children to clean up if they
         have update or childUpdate flagged */
      let drawList =
        List.fold_left(
          (drawList, updChild) =>
            if (updChild.childUpdate) {
              drawListLoop(updChild, drawList, hidden);
            } else if (updChild.update) {
              drawListLoop(updChild, drawList, hidden);
            } else {
              drawList;
            },
          drawList,
          updNode.updChildren
        );
      /* Clean up active stencils */
      List.iter(stencil => stencil.active = false, updNode.stencils);
      drawList;
    } else {
      /* todo: possibly some microoptimizations if either lists are empty */
      /* Check for active stencils */
      let stencils =
        List.filter(
          (stencil: updateStencil) => stencil.active,
          updNode.stencils
        );
      /* Filter duplicate stencils/stencils contained by other stencils */
      let rec nonDupStencils = (list: list(updateStencil)) =>
        switch list {
        | [] => []
        | [stencil, ...rest] =>
          if (List.exists(
                (stencil2: updateStencil) =>
                  Geom2d.Rect.contains(stencil2.rect, stencil.rect),
                rest
              )) {
            nonDupStencils(rest);
          } else {
            [stencil, ...nonDupStencils(rest)];
          }
        };
      let stencils = nonDupStencils(stencils);
      /* Possibly a slight optimization
         would be to, in the case of one rect, draw
         stencil after rect. In the case of several rects,
         the stencils might take part in several of them
         and more logic would be needed.
         Or maybe better a bounding box over rects or node could
         be used to shape the stencils on the cpu */
      let drawList =
        switch stencils {
        | [] =>
          if (activeStencils^ == None) {
            drawList;
          } else {
            activeStencils := None;
            [ClearStencil, ...drawList];
          }
        | stencils =>
          if (activeStencils^ != None && equalsActiveStencils(stencils)) {
            drawList;
          } else {
            /* Create buffers */
            let data =
              Array.concat(
                List.map(
                  (stencil: updateStencil) => {
                    let rect = stencil.rect;
                    [|
                      rect.x,
                      rect.y -. rect.h, /* Bottom left */
                      rect.x,
                      rect.y, /* Top left */
                      rect.x +. rect.w,
                      rect.y, /* Top right */
                      rect.x +. rect.w,
                      rect.y
                      -. rect.h /* Bottom right */
                    |];
                  },
                  stencils
                )
              );
            let vb =
              Gpu.VertexBuffer.makeQuad(~data, ~usage=Gpu.DynamicDraw, ());
            let ib =
              Gpu.IndexBuffer.make(
                Gpu.IndexBuffer.makeQuadsData(List.length(stencils)),
                Gpu.DynamicDraw
              );
            let stencilBuffers = {
              stencilVertices:
                Gpu.VertexBuffer.init(vb, scene.canvas.context, gpuState),
              stencilIndices:
                Gpu.IndexBuffer.init(ib, scene.canvas.context, gpuState)
            };
            activeStencils := Some(stencils);
            [DrawStencil(stencilBuffers), ...drawList];
          }
        };
      if (updNode.update) {
        /* Clean up update flag */
        updNode.update = false;
        /* Ensure node has drawState */
        if (updNode.updNode.drawState == None) {
          createDrawState(scene, updNode.updNode);
        };
        /* Clear active rect if any */
        let drawList =
          if (activeRect^ != None) {
            activeRect := None;
            [ClearRect, ...drawList];
          } else {
            drawList;
          };
        let (drawList, drawToTexture) =
          switch updNode.updNode.drawToTexture {
          | Some(_tex) => (
              [BindDrawTexture(updNode.updNode), ...drawList],
              true
            )
          | None => (drawList, false)
          };
        let drawList = [DrawNode(updNode.updNode), ...drawList];
        /* Since this node is flagged for update, assume all children
           will also be updated */
        let drawList =
          List.fold_left(
            (drawList, updChild) => drawListLoop(updChild, drawList, hidden),
            drawList,
            updNode.updChildren
          );
        let drawList =
          if (drawToTexture) {
            [UnBindDrawTexture, ...drawList];
          } else {
            drawList;
          };
        /* Clean up active stencils */
        List.iter(stencil => stencil.active = false, stencils);
        /* Clean up active rects */
        List.iter(
          (rect: updateRect('s)) => rect.active = false,
          updNode.childRects
        );
        drawList;
      } else {
        /* Check for active rects */
        let rects =
          List.filter(
            (updRect: updateRect('s)) => updRect.active,
            updNode.childRects
          );
        /* Filter duplicate/contained  */
        let rec nonDupRects = (list: list(updateRect('s))) =>
          switch list {
          | [] => []
          | [rect, ...rest] =>
            /* !! Also negates active flag !! */
            rect.active = false;
            if (List.exists(
                  (rect2: updateRect('s)) =>
                    Geom2d.Rect.contains(rect2.rect, rect.rect),
                  rest
                )) {
              nonDupRects(rest);
            } else {
              [rect, ...nonDupRects(rest)];
            };
          };
        let rects = nonDupRects(rects);
        /* When there are multiple rects, another strategy would
           be to set up a stencil buffer with all of them
           and regular stencils subtracted.
           Though I assume in some cases it may not be
           that much better as a scissor rect should be easier
           to deal with for the gpu? So it would be nice to
           see benchmarks */
        let drawList =
          List.fold_left(
            (drawList, rect: updateRect('s)) =>
              if (activeRect^ == None) {
                activeRect := Some(rect.scisRect);
                [
                  DrawNode(updNode.updNode),
                  SetRect(rect.scisRect),
                  ...drawList
                ];
              } else if (equalsActiveRect(rect.scisRect)) {
                [DrawNode(updNode.updNode)];
              } else {
                /* Clear and set new rect */
                activeRect := Some(rect.scisRect);
                [
                  DrawNode(updNode.updNode),
                  SetRect(rect.scisRect),
                  ClearRect,
                  ...drawList
                ];
              },
            drawList,
            rects
          );
        /* Recurse to children if they have update or childUpdate flagged */
        let drawList =
          List.fold_left(
            (drawList, updChild) =>
              if (updChild.update) {
                drawListLoop(updChild, drawList, hidden);
              } else if (updChild.childUpdate) {
                drawListLoop(updChild, drawList, hidden);
              } else {
                drawList;
              },
            drawList,
            updNode.updChildren
          );
        List.iter(stencil => stencil.active = false, stencils);
        drawList;
      };
    };
  };
  /* Not sure if we need to useProgram for stencil buffers */
  Reasongl.Gl.useProgram(
    ~context=scene.canvas.context,
    scene.stencilDraw.program.programRef
  );
  gpuState.curProg = scene.stencilDraw.program.rId;
  let drawList = drawListLoop(updRoot, [], false);
  /* Clear active rect if any */
  let drawList =
    if (activeRect^ != None) {
      [ClearRect, ...drawList];
    } else {
      drawList;
    };
  /* Clear active stencils if any */
  let drawList =
    if (activeStencils^ != None) {
      [ClearStencil, ...drawList];
    } else {
      drawList;
    };
  /* When called with non-root root, for example
     when drawing deps, there can be lingering
     updates set
     Todo: alternative is to not set update
     in these cases, probably better,
     requires passing root around */
  let rec clearChildUpdates = child =>
    if (child.update) {
      child.update = false;
      List.iter(child => clearChildUpdates(child), child.updChildren);
    } else if (child.childUpdate) {
      child.childUpdate = false;
      List.iter(child => clearChildUpdates(child), child.updChildren);
    };
  let rec clearParentUpdates = parent =>
    switch parent {
    | Some(updNode) =>
      if (updNode.childUpdate) {
        updNode.childUpdate = false;
        List.iter(child => clearChildUpdates(child), updNode.updChildren);
      };
      if (updNode.update) {
        updNode.update = false;
        List.iter(child => clearChildUpdates(child), updNode.updChildren);
      };
      clearParentUpdates(updNode.parent);
    | None => ()
    };
  clearParentUpdates(updRoot.parent);
  List.rev(drawList);
};

let getFBuffer = (scene, config: fbufferConfig) =>
  if (Hashtbl.mem(scene.fbuffer, config)) {
    Hashtbl.find(scene.fbuffer, config);
  } else {
    let fbuffer =
      Gpu.FrameBuffer.init(
        Gpu.FrameBuffer.make(config.width, config.height),
        scene.canvas.context
      );
    Hashtbl.add(scene.fbuffer, config, fbuffer);
    fbuffer;
  };

let debugNodes = [];

type drawListDrawTo = {
  dtFbuffer: Gpu.FrameBuffer.inited,
  dtTexture: Gpu.Texture.t,
  dtWidth: int,
  dtHeight: int
};

type drawListState = {
  mutable activeDrawTo: option(drawListDrawTo),
  mutable drawToStack: list(drawListDrawTo)
};

let bindDrawTexture = (scene, node, dlState) =>
  switch node.drawToTexture {
  | Some(texture) =>
    /* If any active drawTo, put in stack */
    switch dlState.activeDrawTo {
    | None => ()
    | Some(drawTo) => dlState.drawToStack = [drawTo, ...dlState.drawToStack]
    };
    let config = {width: 1024, height: 1024};
    let fbuffer = getFBuffer(scene, config);
    Gpu.FrameBuffer._bindFramebuffer(
      ~context=scene.canvas.context,
      Gpu.FrameBuffer.frameBufferC,
      Js.Nullable.return(fbuffer.frameBufferRef)
    );
    /* Let pixels go from offset of node */
    /* Possibly use for this, would need to adjust layouts */
    let dtWidth = int_of_float(node.rect.w);
    let dtHeight = int_of_float(node.rect.h);
    Reasongl.Gl.viewport(
      ~context=scene.canvas.context,
      ~x=0,
      ~y=0,
      ~width=dtWidth,
      ~height=dtHeight
    );
    Gpu.FrameBuffer.bindTexture(fbuffer, scene.canvas.context, texture);
    if (node.clearOnDraw) {
      /* Alpha channel should maybe only cleared on rgba */
      Gpu.Canvas.clear(
        scene.canvas,
        0.0,
        0.0,
        0.0,
        0.0
      );
    };
    dlState.activeDrawTo =
      Some({dtFbuffer: fbuffer, dtTexture: texture, dtWidth, dtHeight});
    let debugBuffered = Some("lightBase");
    let debugBuffered = None;
    switch (debugBuffered, node.key, node.cls) {
    | (Some(d), Some(k), _) when k == d =>
      [%debugger];
      Gpu.Canvas.clearFramebuffer(scene.canvas);
    | (Some(d), _, Some(k)) when k == d =>
      [%debugger];
      Gpu.Canvas.clearFramebuffer(scene.canvas);
    | _ => ()
    };
  | None => failwith("Node is not drawToTexture")
  };

let unbindDrawTexture = (scene, dlState) =>
  switch dlState.drawToStack {
  | [] =>
    dlState.activeDrawTo = None;
    Gpu.Canvas.clearFramebuffer(scene.canvas);
    /* Possibly unneccesary */
    Reasongl.Gl.viewport(
      ~context=scene.canvas.context,
      ~x=0,
      ~y=0,
      ~width=scene.canvas.width,
      ~height=scene.canvas.height
    );
  | [{dtFbuffer, dtTexture, dtWidth, dtHeight} as prevActive, ...rest] =>
    Gpu.FrameBuffer.bindTexture(dtFbuffer, scene.canvas.context, dtTexture);
    Gpu.Canvas.setFramebuffer(scene.canvas, dtFbuffer);
    dlState.drawToStack = rest;
    dlState.activeDrawTo = Some(prevActive);
    Reasongl.Gl.viewport(
      ~context=scene.canvas.context,
      ~x=0,
      ~y=0,
      ~width=dtWidth,
      ~height=dtHeight
    );
  };

let draw = (scene, node) =>
  switch node.drawState {
  | Some(drawState) =>
    switch node.elapsedUniform {
    | Some(elapsedUniform) =>
      Gpu.Uniform.setFloat(elapsedUniform, scene.canvas.elapsed)
    | None => ()
    };
    let isDebug =
      switch (node.key, node.cls) {
      | (Some(key), _) => List.exists(debug => key == debug, debugNodes)
      | (_, Some(cls)) => List.exists(debug => cls == debug, debugNodes)
      | _ => false
      };
    if (isDebug) {
      [%debugger];
    };
    if (node.transparent) {
      let context = scene.canvas.context;
      Gpu.glEnable(context, Gpu.Constants.blend);
      let nodeFactor =
        switch node.blendFactor {
        | BlendAlpha => Gpu.Constants.src_alpha
        | BlendOne => 1
        };
      Gpu.glBlendFunc(context, nodeFactor, Gpu.Constants.one_minus_src_alpha);
      Gpu.DrawState.draw(drawState, scene.canvas);
      Gpu.glDisable(context, Gpu.Constants.blend);
    } else if (isDebug) {
      Gpu.DrawState.draw(drawState, scene.canvas);
      Gpu.Canvas.clearFramebuffer(scene.canvas);
      Gpu.DrawState.draw(drawState, scene.canvas);
    } else {
      Gpu.DrawState.draw(drawState, scene.canvas);
    };
  | None => failwith("Drawstate not found")
  };

module Gl = Reasongl.Gl;

/* Debug parameter passed, it would be a slight optimization
   to create own function for debug.. */
let processDrawList = (scene, drawList, debug) => {
  let context = scene.canvas.context;
  let elapsed = scene.canvas.elapsed;
  let dlState = {activeDrawTo: None, drawToStack: []};
  open Gpu;
  module Gl = Reasongl.Gl;
  let rec processDrawEl = list =>
    switch list {
    | [el, ...rest] =>
      switch el {
      | DrawNode(node) =>
        if (debug) {
          switch (scene.drawListsDebug, node.layoutUniform, node.drawToTexture) {
          | (Some(drawDebug), Some(UniformMat3f(uniformMat)), None) =>
            DrawListDebug.draw(
              drawDebug.draw,
              uniformMat^,
              elapsed +. float_of_int(node.id)
            )
          | _ => ()
          };
        } else if (node.loading) {
          if (! List.exists(loading => loading === node, scene.loadingNodes)) {
            scene.loadingNodes = [node, ...scene.loadingNodes];
          };
        } else {
          switch node.onUpdate {
          | Some(update) => [@bs] update(node, scene.state)
          | None => draw(scene, node)
          };
        }
      | DrawStencil(stencilBuffers) =>
        glEnable(context, Stencil.stencilTest);
        Canvas.clearStencil(scene.canvas);
        /* Always write 1s */
        Stencil.stencilFunc(context, Stencil.always, 1, 0xFF);
        Stencil.stencilOp(
          context,
          Stencil.keep,
          Stencil.keep,
          Stencil.replace
        );
        Stencil.stencilMask(context, 0xFF);
        /* Disable color and depth writing when writing
           stencil rects */
        Canvas.colorMask(context, false, false, false, false);
        Canvas.depthMask(context, false);
        StencilDraw.draw(
          scene.stencilDraw,
          stencilBuffers.stencilVertices,
          stencilBuffers.stencilIndices
        );
        Canvas.colorMask(context, true, true, true, true);
        Canvas.depthMask(context, true);
        /* Set stencil test function to check for 1's
           when drawing nodes that should be affected
           by the stencil */
        Stencil.stencilFunc(context, Stencil.notEqual, 1, 0xFF);
        /* We can also disable stencil writing */
        Stencil.stencilMask(context, 0x00);
      | ClearStencil => glDisable(context, Stencil.stencilTest)
      | SetRect(rect) =>
        glEnable(context, Scissor.scissorTest);
        /* todo: Int rect */
        Scissor.scissor(
          context,
          int_of_float(rect.x),
          int_of_float(rect.y),
          int_of_float(rect.w),
          int_of_float(rect.h)
        );
      | ClearRect => glDisable(context, Scissor.scissorTest)
      | BindDrawTexture(n) =>
        if (! debug) {
          bindDrawTexture(scene, n, dlState);
        }
      | UnBindDrawTexture =>
        if (! debug) {
          unbindDrawTexture(scene, dlState);
        }
      };
      processDrawEl(rest);
    | [] => ()
    };
  processDrawEl(drawList);
};

let logDrawList = (scene, updateState: updateState, drawList) => {
  Js.log2(
    "====\nDrawlist",
    List.fold_left(
      (str, id) =>
        switch scene.updateNodes[id] {
        | Some(node) => str ++ ", " ++ nodeDescr(node.updNode)
        | None => str
        },
      "",
      updateState.updateNodes
    )
  );
  let rec processDrawEl = list =>
    switch list {
    | [el, ...rest] =>
      switch el {
      | DrawNode(node) => Js.log("Draw node: " ++ nodeDescr(node))
      | DrawStencil(stencilBuffers) =>
        Js.log2("Draw stencil: ", stencilBuffers.stencilVertices.data)
      | ClearStencil => Js.log("Clear stencil")
      | SetRect(rect) => Js.log2("Set rect: ", rect)
      | ClearRect => Js.log("Clear rect")
      | BindDrawTexture(n) => Js.log("Bind draw texture: " ++ nodeDescr(n))
      | UnBindDrawTexture => Js.log("Unbind draw texture")
      };
      processDrawEl(rest);
    | [] => ()
    };
  processDrawEl(drawList);
};

let getSceneNodesToUpdate = (animIds, root) => {
  let rec loop = (node, list, parentUpdate) => {
    let depsToUpdate =
      List.fold_left((list, dep) => loop(dep, list, false), [], node.deps);
    let doUpdate =
      parentUpdate
      || List.length(depsToUpdate) > 0
      || List.exists(animId => node.id == animId, animIds);
    /* todo: tail recursive? */
    let childList =
      List.fold_left(
        (list, child) => loop(child, list, doUpdate),
        list,
        node.children
      );
    let list =
      if (doUpdate && node.selfDraw) {
        [node, ...childList];
      } else {
        childList;
      };
    /* Deps first */
    List.fold_left((list, dep) => [dep, ...list], list, depsToUpdate);
  };
  loop(root, [], false);
};

let getNode = (scene, key) =>
  if (Hashtbl.mem(scene.nodesByKey, key)) {
    Some(Hashtbl.find(scene.nodesByKey, key));
  } else {
    None;
  };

let getNodeUnsafe = (scene, nodeKey) =>
  switch (getNode(scene, nodeKey)) {
  | Some(node) => node
  | None => failwith("Could not find node: " ++ nodeKey)
  };

let hideNode = (scene, node) => {
  node.hidden = true;
  scene.hiddenNodes =
    List.sort_uniq(
      (a, b) => a > b ? 1 : (-1),
      [node.id, ...scene.hiddenNodes]
    );
  /* todo: Queuing root for now to cover updated area,
     but this could be more precise. Optimally find covering
     area and redraw */
  queueUpdates(scene, [scene.root.id]);
};

let hideNodeByKey = (scene, nodeKey) =>
  switch (getNode(scene, nodeKey)) {
  | Some(node) => hideNode(scene, node)
  | None => failwith("Could not find node: " ++ nodeKey)
  };

let showNode = (scene, node) => {
  node.hidden = false;
  scene.hiddenNodes = List.filter(n => n != node.id, scene.hiddenNodes);
  queueUpdates(scene, [node.id]);
};

let showNodeByKey = (scene, nodeKey) =>
  switch (getNode(scene, nodeKey)) {
  | Some(node) => showNode(scene, node)
  | None => failwith("Could not find node: " ++ nodeKey)
  };

type parentDrawToTex = {
  nOffX: float,
  nOffY: float,
  texVpWidth: float,
  texVpHeight: float
};

/* Todo: More just in time, and fine grained updates */
let calcLayout = scene => {
  let debug = true;
  /* These are assumed "ints"/rounded */
  let vpWidth = float_of_int(scene.canvas.width);
  let vpHeight = float_of_int(scene.canvas.height);
  let calcMargins = (node, outerWidth, outerHeight) => {
    /* Calc margins. Not sure how best to handle
       all cases. Aspect I guess should be kept
       inside the margins, currently the
       margins are scaled by padded dimensions
       around it */
    let cl = node.cLayout;
    /* Calc margins and dimensions inside margin */
    let xdim = dim =>
      switch dim {
      | Scale(scale) => outerWidth *. scale
      | ScreenScale(scale) => vpWidth *. scale
      | Pixel(pixels) => pixels
      };
    let ydim = dim =>
      switch dim {
      | Scale(scale) => outerHeight *. scale
      | ScreenScale(scale) => vpHeight *. scale
      | Pixel(pixels) => pixels
      };
    switch node.layout.margin {
    | None => ()
    | Some(margin) =>
      switch margin {
      | Margin(dimension) =>
        switch dimension {
        | Scale(scale) =>
          let scaledX = outerWidth *. scale;
          let scaledY = outerHeight *. scale;
          cl.marginX1 = scaledX;
          cl.marginX2 = scaledX;
          cl.marginY1 = scaledY;
          cl.marginY2 = scaledY;
        | ScreenScale(scale) =>
          let scaledX = vpWidth *. scale;
          let scaledY = vpHeight *. scale;
          cl.marginX1 = scaledX;
          cl.marginX2 = scaledX;
          cl.marginY1 = scaledY;
          cl.marginY2 = scaledY;
        | Pixel(pixels) =>
          cl.marginX1 = pixels;
          cl.marginX2 = pixels;
          cl.marginY1 = pixels;
          cl.marginY2 = pixels;
        }
      | MarginXY(dimX, dimY) =>
        let dimX = xdim(dimX);
        let dimY = ydim(dimY);
        cl.marginX1 = dimX;
        cl.marginX2 = dimX;
        cl.marginY1 = dimY;
        cl.marginY2 = dimY;
      | MarginRBLT(mRight, mBottom, mLeft, mTop) =>
        let dimX2 = xdim(mRight);
        let dimY2 = ydim(mBottom);
        let dimX1 = xdim(mLeft);
        let dimY1 = ydim(mTop);
        cl.marginX1 = dimX1;
        cl.marginX2 = dimX2;
        cl.marginY1 = dimY1;
        cl.marginY2 = dimY2;
      }
    };
  };
  let calcNodeDimensions = (node, paddedWidth, paddedHeight) => {
    calcMargins(node, paddedWidth, paddedHeight);
    let cl = node.cLayout;
    let (nodeWidth, nodeHeight) =
      switch node.layout.size {
      | Aspect(ratio) =>
        switch node.layout.margin {
        | Some(_) =>
          /* Keeping aspect inside margins */
          let innerWidth = paddedWidth -. cl.marginX1 -. cl.marginX2;
          let innerHeight = paddedHeight -. cl.marginY1 -. cl.marginY2;
          let parentAspect = innerWidth /. innerHeight;
          if (ratio < parentAspect) {
            /* Limit by height */
            let innerWidth = innerHeight *. ratio;
            (innerWidth +. cl.marginX1 +. cl.marginX2, paddedHeight);
          } else {
            /* Limit by width */
            let innerHeight = innerWidth /. ratio;
            (paddedWidth, innerHeight +. cl.marginY1 +. cl.marginY2);
          };
        | None =>
          let parentAspect = paddedWidth /. paddedHeight;
          if (ratio < parentAspect) {
            /* Limit by height */
            let width = paddedHeight *. ratio;
            (width, paddedHeight);
          } else {
            /* Limit by width */
            let height = paddedWidth /. ratio;
            (paddedWidth, height);
          };
        }
      | Dimensions(dimX, dimY) =>
        /* Get in pixel or ratio form */
        (
          switch dimX {
          | Pixel(pixels) => pixels
          | Scale(scale) => paddedWidth *. scale
          | ScreenScale(scale) => vpWidth *. scale
          },
          switch dimY {
          | Pixel(pixels) => pixels
          | Scale(scale) => paddedHeight *. scale
          | ScreenScale(scale) => vpWidth *. scale
          }
        )
      | WidthRatio(dimX, ratio) =>
        let width =
          switch dimX {
          | Pixel(pixels) => pixels
          | Scale(scale) => paddedWidth *. scale
          | ScreenScale(scale) => vpWidth *. scale
          };
        (width, width /. ratio);
      | HeightRatio(dimY, ratio) =>
        let height =
          switch dimY {
          | Pixel(pixels) => pixels
          | Scale(scale) => paddedHeight *. scale
          | ScreenScale(scale) => vpHeight *. scale
          };
        (height *. ratio, height);
      };
    cl.pWidth = nodeWidth;
    cl.pHeight = nodeHeight;
    cl.inWidth = cl.pWidth -. cl.marginX1 -. cl.marginX2;
    cl.inHeight = cl.pHeight -. cl.marginY1 -. cl.marginY2;
  };
  let rec calcNodeLayout = (node, parentDrawToTex) => {
    let layout = node.layout;
    let cl = node.cLayout;
    /* Calc padding */
    /* x and y is used in child layout calculations */
    let (paddedWidth, paddedHeight, x, y) =
      switch layout.padding {
      | None => (
          cl.pWidth,
          cl.pHeight,
          cl.pXOffset,
          cl.pYOffset
        )
      | Some(Pixel(padding)) => (
          cl.pWidth -. padding *. 2.0,
          cl.pHeight -. padding *. 2.0,
          cl.pXOffset +. padding,
          cl.pYOffset +. padding
        )
      | Some(ScreenScale(padding)) =>
        let scaledXPadding = vpWidth *. padding;
        let scaledYPadding = vpHeight *. padding;
        (
          cl.pWidth -. scaledXPadding *. 2.0,
          cl.pHeight -. scaledYPadding *. 2.0,
          cl.pXOffset +. scaledXPadding,
          cl.pYOffset +. scaledYPadding
        );
      | Some(Scale(padding)) =>
        let scaledXPadding = cl.pWidth *. padding;
        let scaledYPadding = cl.pHeight *. padding;
        (
          cl.pWidth -. scaledXPadding *. 2.0,
          cl.pHeight -. scaledYPadding *. 2.0,
          cl.pXOffset +. scaledXPadding,
          cl.pYOffset +. scaledYPadding
        );
      };
    let x = x +. cl.marginX1;
    let y = y +. cl.marginY1;
    /* Set width/height of children and deps */
    let childWidth = paddedWidth -. cl.marginX1 -. cl.marginX2;
    let childHeight =
      paddedHeight -. cl.marginY1 -. cl.marginY2;
    List.iter(
      dep => calcNodeDimensions(dep, childWidth, childHeight),
      node.deps
    );
    List.iter(
      child => calcNodeDimensions(child, childWidth, childHeight),
      node.children
    );
    /* Set x and y offset for deps */
    List.iter(
      dep => {
        dep.cLayout.pXOffset = x;
        dep.cLayout.pYOffset = y;
      },
      node.deps
    );
    /* Todo: allow to set a pixel value or something for one child,
       then allow one of the other elements to stretch to available space */
    /* Handle aligns */
    switch layout.childLayout {
    | Stacked =>
      switch layout.hAlign {
      | AlignLeft =>
        List.iter(
          child => child.cLayout.pXOffset = x +. child.cLayout.marginX1,
          node.children
        )
      | AlignCenter =>
        List.iter(
          child =>
            child.cLayout.pXOffset =
              x
              +. (childWidth -. child.cLayout.pWidth)
              /. 2.0
              +. child.cLayout.marginX1,
          node.children
        )
      | AlignRight =>
        List.iter(
          child =>
            child.cLayout.pXOffset =
              x
              +. childWidth
              -. child.cLayout.pWidth
              +. child.cLayout.marginX1,
          node.children
        )
      };
      switch layout.vAlign {
      | AlignTop =>
        List.iter(
          child => child.cLayout.pYOffset = y +. child.cLayout.marginY1,
          node.children
        )
      | AlignMiddle =>
        List.iter(
          child =>
            child.cLayout.pYOffset =
              y
              +. (childHeight -. child.cLayout.pHeight)
              /. 2.0
              +. child.cLayout.marginY1,
          node.children
        )
      | AlignBottom =>
        List.iter(
          child =>
            child.cLayout.pYOffset =
              y
              +. childHeight
              -. child.cLayout.pHeight
              +. child.cLayout.marginY1,
          node.children
        )
      };
    | Horizontal =>
      /* Use ratio as scale. Not sure if it makes total sense,
         possibly restrict to width, height dimensions */
      let spacing =
        switch layout.spacing {
        | Some(Pixel(pixel)) => pixel
        | Some(Scale(scale)) => childWidth *. scale
        | Some(ScreenScale(scale)) => vpWidth *. scale
        | None => 0.0
        };
      /* Set xoffset */
      switch layout.hAlign {
      | AlignLeft =>
        let _ =
          List.fold_left(
            (xOffset, child) => {
              child.cLayout.pXOffset = xOffset +. child.cLayout.marginX1;
              xOffset +. spacing +. child.cLayout.pWidth;
            },
            x,
            node.children
          );
        ();
      | AlignCenter =>
        /* Get total width and start from (paddedWidth  - totalWidth) / 2) */
        let totalWidth =
          List.fold_left(
            (totalWidth, child) => totalWidth +. child.cLayout.pWidth,
            0.0,
            node.children
          )
          +. spacing
          *. float_of_int(List.length(node.children) - 1);
        let xOffset = (childWidth -. totalWidth) /. 2.0;
        let _ =
          List.fold_left(
            (xOffset, child) => {
              child.cLayout.pXOffset = xOffset +. child.cLayout.marginX1;
              xOffset +. spacing +. child.cLayout.pWidth;
            },
            x +. xOffset,
            node.children
          );
        ();
      | AlignRight =>
        let _ =
          List.fold_right(
            (child, xOffset) => {
              let childOffset = xOffset -. child.cLayout.pWidth;
              child.cLayout.pXOffset =
                childOffset +. child.cLayout.marginX1;
              childOffset -. spacing;
            },
            node.children,
            x +. childWidth
          );
        ();
      };
      switch layout.vAlign {
      | AlignTop =>
        List.iter(
          child => child.cLayout.pYOffset = y +. child.cLayout.marginY1,
          node.children
        )
      | AlignMiddle =>
        List.iter(
          child =>
            child.cLayout.pYOffset =
              y
              +. (childHeight -. child.cLayout.pHeight)
              /. 2.0
              +. child.cLayout.marginY1,
          node.children
        )
      | AlignBottom =>
        List.iter(
          child =>
            child.cLayout.pYOffset =
              y
              +. childHeight
              -. child.cLayout.pHeight
              +. child.cLayout.marginY1,
          node.children
        )
      };
    /* Get total width */
    | Vertical =>
      let spacing =
        switch layout.spacing {
        | Some(Pixel(pixel)) => pixel
        | Some(Scale(scale)) => childHeight *. scale
        | Some(ScreenScale(scale)) => vpHeight *. scale
        | None => 0.0
        };
      switch layout.hAlign {
      | AlignLeft =>
        List.iter(
          child => child.cLayout.pXOffset = x +. child.cLayout.marginX1,
          node.children
        )
      | AlignCenter =>
        List.iter(
          child =>
            child.cLayout.pXOffset =
              x
              +. (childWidth -. child.cLayout.pWidth)
              /. 2.0
              +. child.cLayout.marginX1,
          node.children
        )
      | AlignRight =>
        List.iter(
          child =>
            child.cLayout.pXOffset =
              x
              +. childWidth
              -. child.cLayout.pWidth
              +. child.cLayout.marginX1,
          node.children
        )
      };
      switch layout.vAlign {
      | AlignTop =>
        let _ =
          List.fold_left(
            (yOffset, child) => {
              child.cLayout.pYOffset = yOffset +. child.cLayout.marginY1;
              yOffset +. child.cLayout.pHeight +. spacing;
            },
            y,
            node.children
          );
        ();
      | AlignMiddle =>
        let totalHeight =
          List.fold_left(
            (totalHeight, child) => totalHeight +. child.cLayout.pHeight,
            0.0,
            node.children
          )
          +. spacing
          *. float_of_int(List.length(node.children) - 1);
        let _ =
          List.fold_left(
            (yOffset, child) => {
              child.cLayout.pYOffset = yOffset +. child.cLayout.marginY1;
              yOffset +. child.cLayout.pHeight +. spacing;
            },
            y +. (childHeight -. totalHeight) /. 2.0,
            node.children
          );
        ();
      | AlignBottom =>
        let _ =
          List.fold_right(
            (child, yOffset) => {
              let childOffset = yOffset -. child.cLayout.pHeight;
              child.cLayout.pYOffset =
                childOffset +. child.cLayout.marginY1;
              childOffset -. spacing;
            },
            node.children,
            y +. childHeight
          );
        ();
      };
    };
    /* Round numbers to make them work as pixel values
       since they are used with rects etc and nodes
       are drawn independently */
    let pXOff = floor(cl.pXOffset +. 0.5);
    let pYOff = floor(cl.pYOffset +. 0.5);
    let inW =
      floor(cl.inWidth +. (cl.pXOffset -. pXOff) +. 0.5);
    let inH =
      floor(cl.inHeight +. (cl.pYOffset -. pYOff) +. 0.5);
    let pW = floor(cl.pWidth +. (cl.pXOffset -. pXOff) +. 0.5);
    let pH =
      floor(cl.pHeight +. (cl.pYOffset -. pYOff) +. 0.5);
    node.rect.x = pXOff;
    node.rect.y = pYOff;
    node.rect.w = inW;
    node.rect.h = inH;
    node.outRect.x = floor(cl.pXOffset -. cl.marginX1 +. 0.5);
    node.outRect.y = floor(cl.pYOffset -. cl.marginY1 +. 0.5);
    node.outRect.w = pW;
    node.outRect.h = pH;
    node.scissorRect.x = pXOff;
    node.scissorRect.y = vpHeight -. pYOff -. inH;
    node.scissorRect.w = inW;
    node.scissorRect.h = inH;
    /* Bit torn on whether to do this for all nodes,
       currently used for stencils, possibly there
       are other uses, but generally layout matrix is also available.
       We have vpWidth available, if calculations
       are moved, vpWidth etc might beneficially
       be stored. */
    node.screenRect.x = pXOff /. vpWidth *. 2.0 -. 1.0;
    node.screenRect.y = 1.0 -. pYOff /. vpHeight *. 2.0;
    node.screenRect.w = inW /. vpWidth *. 2.0;
    node.screenRect.h = inH /. vpHeight *. 2.0;
    if (debug) {
      Js.log("Layout for " ++ nodeDescr(node));
      Js.log2("pWidth: ", cl.pWidth);
      Js.log2("pHeight: ", cl.pHeight);
      Js.log2("xOff: ", cl.pXOffset);
      Js.log2("yOff: ", cl.pYOffset);
    };
    let parentDrawToTex =
      switch node.layoutUniform {
      | None => parentDrawToTex
      | Some(layoutUniform) =>
        let scaleX = inW /. vpWidth;
        let scaleY = inH /. vpHeight;
        switch (node.drawToTexture, node.texTransUniform) {
        | (Some(texture), None) =>
          /* Ensuring texture has size to hold result */
          Gpu.Texture.ensureSize(
            texture,
            scene.canvas.context,
            int_of_float(node.rect.w),
            int_of_float(node.rect.h)
          );
          /* Translate texture to begin from 0,0.
             The important thing is correspondance with texture
             uniforms below */
          /* Todo: maintain values for quick lookup? */
          /* Todo: Pack texture into available space,
             resize texture on need?
             http://blog.roomanna.com/09-25-2015/binpacking-shelf
             */
          /*
           let (texWidth, texHeight) = switch (Gpu.Texture.getSize(texture)) {
           | Some((texWidth, texHeight)) => (float_of_int(texWidth), float_of_int(texHeight))
           | None => failwith("Could not find texture size");
           };
           */
          let layoutMat = Data.Mat3.scaleTrans(1.0, 1.0, 0.0, 0.0);
          Gpu.Uniform.setMat3f(layoutUniform, layoutMat);
          if (debug) {
            Js.log2("Layout: ", layoutMat);
          };
          /* Initiate parentTexTrans */
          Some({
            nOffX: pXOff,
            nOffY: pYOff,
            texVpWidth: inW,
            texVpHeight: inH
          });
        | (None, _) =>
          /* todo: case of None + texTransform */
          let transX = (pXOff *. 2.0 +. inW) /. vpWidth -. 1.0;
          let transY = (pYOff *. (-2.0) -. inH) /. vpHeight +. 1.0;
          let layoutMat =
            switch parentDrawToTex {
            | None => Data.Mat3.scaleTrans(scaleX, scaleY, transX, transY)
            | Some(pt) =>
              Data.Mat3.scaleTrans(
                inW /. pt.texVpWidth,
                inH /. pt.texVpHeight,
                ((pXOff -. pt.nOffX) *. 2.0 +. inW) /. pt.texVpWidth -. 1.0,
                ((pYOff -. pt.nOffY) *. (-2.0) -. inH) /. pt.texVpHeight +. 1.0
              )
            };
          Gpu.Uniform.setMat3f(layoutUniform, layoutMat);
          if (debug) {
            Js.log2("Layout: ", layoutMat);
          };
          parentDrawToTex;
        | (Some(texture), Some(texTransUniform)) =>
          /* Ensuring texture has size to hold result */
          Gpu.Texture.ensureSize(
            texture,
            scene.canvas.context,
            int_of_float(node.rect.w),
            int_of_float(node.rect.h)
          );
          /* First regular layout uniform */
          let transX = (pXOff *. 2.0 +. inW) /. vpWidth -. 1.0;
          let transY = (pYOff *. (-2.0) -. inH) /. vpHeight +. 1.0;
          let layoutMat = Data.Mat3.scaleTrans(scaleX, scaleY, transX, transY);
          Gpu.Uniform.setMat3f(layoutUniform, layoutMat);
          /* texTransUniform */
          let texTrans = Data.Mat3.scaleTrans(1.0, 1.0, 0.0, 0.0);
          Gpu.Uniform.setMat3f(texTransUniform, texTrans);
          /* Initiate parent tex transform */
          Some({
            nOffX: pXOff,
            nOffY: pYOff,
            texVpWidth: inW,
            texVpHeight: inH
          });
        };
      };
    switch node.pixelSizeUniform {
    | None => ()
    | Some(pixelSizeUniform) =>
      Gpu.Uniform.setVec2f(pixelSizeUniform, Data.Vec2.make(inW, inH))
    };
    /* Iter deps */
    List.iter(dep => calcNodeLayout(dep, None), node.deps);
    /* Iter children */
    List.iter(child => calcNodeLayout(child, parentDrawToTex), node.children);
    /* After deps and children have been processed, set texture matrices */
    Hashtbl.iter(
      (_name, (nodeTex: sceneTexture, uniform)) =>
        /* Textures should be scaled for right pixel ratio,
           need not be translated (could be packed maybe),
           but should start from beginning or pack position */
        /* Understand this as texture size goes to 1.0 on viewport size
           so it will depend on setting viewport before
           render to texture */
        switch nodeTex.texNode {
        | Some(texNode) =>
          let texNode =
            switch scene.updateNodes[texNode] {
            | Some(texNode) => texNode.updNode
            | None => failwith("Could not find texNode")
            };
          /* todo: Can we be sure this is available?
             It is currently resized in this traversal.
             Alternatively loop through these
             after layout + ensure layout is done */
          let (texW, texH) =
            switch (Gpu.Texture.getSize(nodeTex.texture)) {
            | Some(wh) => wh
            | None => failwith("Could not get size of texture")
            };
          let scaleX =
            texNode.rect.w
            /. float_of_int(texW)
            /. 2.0
            *. (texNode.rect.w /. node.rect.w);
          let scaleY =
            texNode.rect.h
            /. float_of_int(texH)
            /. 2.0
            *. (texNode.rect.h /. node.rect.h);
          let texMat = Data.Mat3.scaleTrans(scaleX, scaleY, scaleX, scaleY);
          Gpu.Uniform.setMat3f(uniform, texMat);
        | _ => ()
        },
      node.texMats
    );
  };
  if (debug) {
    Js.log2("vpWidth", vpWidth);
    Js.log2("vpHeight", vpHeight);
  };
  calcNodeDimensions(scene.root, vpWidth, vpHeight);
  scene.root.cLayout.pXOffset = 0.0;
  scene.root.cLayout.pYOffset = 0.0;
  calcNodeLayout(scene.root, None);
};

/* Returns list of dep node ids */
let collectDeps = (scene, nodeIds, hiddenNodes) => {
  let rec loop = (node, list) =>
    if (List.exists(hiddenId => hiddenId == node.id, hiddenNodes)) {
      list;
    } else {
      /* Add deps */
      let list =
        List.fold_left((list, dep) => [dep.id, ...list], list, node.deps);
      /* Traverse children */
      List.fold_left((list, child) => loop(child, list), list, node.children);
    };
  /* Reversing so deps are processesed in the
     order they are listed */
  List.rev(
    List.fold_left(
      (list, nodeId) => {
        let node =
          switch scene.updateNodes[nodeId] {
          | Some(updNode) => updNode.updNode
          | None => failwith("Could not find node")
          };
        loop(node, list);
      },
      [],
      nodeIds
    )
  );
};

/* Checks node and parents for hidden flag
   Would feel a little better to take in
   a list of hidden nodes, on the other
   hand this is probably more efficient */
let isNodeHidden = (scene, nodeId) => {
  let rec loop = node =>
    if (node.hidden) {
      true;
    } else {
      switch node.parent {
      | Some(parent) => loop(parent)
      | None => false
      };
    };
  switch scene.updateNodes[nodeId] {
  | Some(node) => loop(node.updNode)
  | None => failwith("Could not find node")
  };
};

/* Debug function to ensure state is reset after processing of tree */
let rec checkForCleanState = updNode => {
  if (updNode.update || updNode.childUpdate) {
    [%debugger];
  };
  List.iter(
    stencil =>
      if (stencil.active) {
        [%debugger];
      },
    updNode.stencils
  );
  List.iter(
    (rect: updateRect('s)) =>
      if (rect.active) {
        [%debugger];
      },
    updNode.childRects
  );
  List.iter(dep => checkForCleanState(dep), updNode.updDeps);
  List.iter(child => checkForCleanState(child), updNode.updChildren);
};

/* Creates a drawList that shows which
   areas are updating */
let update = scene => {
  /* Anims */
  let rec doAnims = anims =>
    switch anims {
    | [] => []
    | [anim, ...rest] =>
      anim.elapsed = anim.elapsed +. scene.canvas.deltaTime;
      if (anim.frameInterval == 1) {
        [@bs] anim.onFrame(scene, anim);
      } else if (anim.numFrames mod anim.frameInterval == 0) {
        [@bs] anim.onFrame(scene, anim);
      };
      anim.numFrames = anim.numFrames + 1;
      if (anim.elapsed >= anim.duration) {
        [@bs] anim.setLast(scene);
        /* Probably this should be done after update, between frames */
        switch anim.onDone {
        | None => ()
        | Some(onDone) => onDone(scene)
        };
        doAnims(rest);
      } else {
        [anim, ...doAnims(rest)];
      };
    };
  scene.anims = doAnims(scene.anims);
  /* Queued nodes, these are queued because of
     updates in uniforms, vertices or textures */
  let updateNodes =
    List.sort_uniq(
      (a, b) => a < b ? (-1) : a > b ? 1 : 0,
      scene.queuedUpdates
    );
  /* todo: Consider filtering hidden node ids.
     Those hidden may be hidden below another hidden node,
     requiring some traversal. Maybe a cache could be used,
     maybe it's not so bad as the list should be similar
     when things update anyway, and drawlist is cached */
  scene.queuedUpdates = [];
  let updateState = {updateNodes, hiddenNodes: scene.hiddenNodes};
  /* Ensure state is inited */
  let rec initDeps = (nodeId, blackList) => {
    let depIds = collectDeps(scene, [nodeId], scene.hiddenNodes);
    List.iter(
      depId =>
        if (! Hashtbl.mem(scene.initedDeps, depId)) {
          Hashtbl.add(scene.initedDeps, depId, true);
          /* Recurse for deps of this dep */
          initDeps(depId, []);
          /* If dep is in list of update nodes, let other
             drawlist include dep.
             However, this will not work for deps of deps,
             as they need to be loaded first */
          if (! List.exists(nodeId => nodeId == depId, blackList)) {
            switch scene.updateNodes[depId] {
            | Some(depNode) =>
              let depDraws = createDrawList(scene, [depId], depNode.updNode);
              processDrawList(scene, depDraws, false);
              switch scene.drawListsDebug {
              | None => ()
              | Some(_listsDebug) =>
                Js.log("==\nDep drawlist: " ++ nodeDescr(depNode.updNode));
                logDrawList(scene, updateState, depDraws);
                checkForCleanState(depNode);
              };
            | None => failwith("Could not find dep node")
            };
          };
        },
      depIds
    );
  };
  /* Both here and when creating drawList, we could
     use filtered list of visible nodes,
     but this is called 60 times a second so we don't want to
     do it when uneccesary, so we keep an option around */
  let visibleNodes =
    if (! Hashtbl.mem(scene.initedLists, updateState)) {
      let visibleNodes =
        List.filter(nodeId => ! isNodeHidden(scene, nodeId), updateNodes);
      Hashtbl.add(scene.initedLists, updateState, true);
      List.iter(
        nodeId =>
          /* If node is itself a dep, don't pass updateNodes as blacklist
             I *think* this is correct, not sure if we could use blacklist
             in some instances
             Also microoptimization could be to reduce updNode lookups */
          switch scene.updateNodes[nodeId] {
          | Some(updNode) =>
            if (updNode.isDep) {
              initDeps(nodeId, []);
            } else {
              initDeps(nodeId, updateNodes);
            }
          | None => failwith("Could not find node")
          },
        visibleNodes
      );
      Some(visibleNodes);
    } else {
      None;
    };
  /*
   Js.log2("== Drawing", List.fold_left((str, id) => {
       switch (scene.updateNodes[id]) {
       | Some(node) => str ++ ", " ++ node.updNode.key
       | None => str
       }
   }, "", updateState.updateNodes));*/
  if (! Hashtbl.mem(scene.drawLists, updateState)) {
    let visibleNodes =
      switch visibleNodes {
      | None =>
        List.filter(nodeId => ! isNodeHidden(scene, nodeId), updateNodes)
      | Some(visibleNodes) => visibleNodes
      };
    let drawList = createDrawList(scene, visibleNodes, scene.root);
    Hashtbl.add(scene.drawLists, updateState, drawList);
    switch scene.drawListsDebug {
    | None => ()
    | Some(_listsDebug) =>
      logDrawList(
        scene,
        updateState,
        Hashtbl.find(scene.drawLists, updateState)
      )
    };
  };
  /* Check if any node in loadingNodes is loaded */
  let (newList, loaded) =
    List.partition(node => node.loading, scene.loadingNodes);
  scene.loadingNodes = newList;
  if (List.length(loaded) > 0) {
    let loadedIds =
      List.map(
        node => {
          switch node.onUpdate {
          | Some(update) => [@bs] update(node, scene.state)
          | None => ()
          };
          node.id;
        },
        loaded
      );
    let drawList = createDrawList(scene, loadedIds, scene.root);
    /* Drawing loaded nodes in a special drawlist
       The loaded node will redraw if they are
       in the main drawlist anyway, todo */
    processDrawList(scene, drawList, false);
  };
  /* todo: possibly optimize with a second transformed data structure
     so the drawstate etc is readily available */
  processDrawList(scene, Hashtbl.find(scene.drawLists, updateState), false);
  switch scene.drawListsDebug {
  | None => ()
  | Some(_) =>
    switch scene.updateNodes[scene.root.id] {
    | None => ()
    | Some(updRoot) => checkForCleanState(updRoot)
    };
    /* When debugging, we could go through list of previous
       draws and repaint with each debugNode knowing how long
       since last paint, and maybe setting alpha
       based on it */
    let context = scene.canvas.context;
    Gpu.glEnable(context, Gpu.Constants.blend);
    Gpu.glBlendFunc(
      context,
      Gpu.Constants.src_alpha,
      Gpu.Constants.one_minus_src_alpha
    );
    processDrawList(scene, Hashtbl.find(scene.drawLists, updateState), true);
    Gpu.glDisable(context, Gpu.Constants.blend);
  };
  /* Check for queued deps then drawStates and process if there are */
  switch scene.queuedDeps {
  | [] =>
    switch scene.queuedDrawStates {
    | [] => ()
    | [next, ...rest] =>
      scene.queuedDrawStates = rest;
      createDrawState(scene, next);
    }
  | [depId, ...rest] =>
    scene.queuedDeps = rest;
    if (! Hashtbl.mem(scene.initedDeps, depId)) {
      Hashtbl.add(scene.initedDeps, depId, true);
      switch scene.updateNodes[depId] {
      | Some(depNode) =>
        let depDraws = createDrawList(scene, [depId], depNode.updNode);
        processDrawList(scene, depDraws, false);
        switch scene.drawListsDebug {
        | None => ()
        | Some(_listsDebug) =>
          Js.log("==\nQueued dep drawlist: " ++ nodeDescr(depNode.updNode));
          logDrawList(scene, {updateNodes: [], hiddenNodes: []}, depDraws);
          checkForCleanState(depNode);
        };
      | None => failwith("Could not find dep node")
      };
    };
  };
};

let cleanUpDrawList = (scene, drawList) => {
  let rec loop = list =>
    switch list {
    | [el, ...rest] =>
      switch el {
      | DrawNode(_node) => ()
      | DrawStencil(stencilBuffers) =>
        /* Clear gpu memory */
        Gpu.VertexBuffer.deleteBuffer(
          scene.canvas.context,
          stencilBuffers.stencilVertices
        );
        Gpu.IndexBuffer.deleteBuffer(
          scene.canvas.context,
          stencilBuffers.stencilIndices
        );
      | ClearStencil => ()
      | SetRect(_rect) => ()
      | ClearRect => ()
      | BindDrawTexture(_) => ()
      | UnBindDrawTexture => ()
      };
      loop(rest);
    | [] => ()
    };
  loop(drawList);
};

let queueDeps = (scene, fromNodeId) => {
  /* We need the deps of deps to be drawn first */
  let rec collectQueue = idList => {
    let depIds = collectDeps(scene, idList, []);
    switch depIds {
    | [] => []
    | list => List.append(collectQueue(depIds), list)
    };
  };
  scene.queuedDeps = collectQueue([fromNodeId]);
};

let doResize = scene => {
  let (width, height) = Gpu.Canvas.getViewportSize();
  Gpu.Canvas.resize(scene.canvas, width, height);
  /* Although rects are updated through cLayout, we
     simply reset the drawList cache for some time.
     Stencil vertices would need to be updated/reset,
     otherwise it would work I think to keep cache */
  Hashtbl.iter(
    (_, drawList) => cleanUpDrawList(scene, drawList),
    scene.drawLists
  );
  Hashtbl.clear(scene.drawLists);
  Hashtbl.clear(scene.initedLists);
  Hashtbl.clear(scene.initedDeps);
  /* Doing full layout, it would be nice to optimize
     as time goes by */
  calcLayout(scene);
  queueUpdates(scene, [scene.root.id]);
  /* Not sure how well it fits to do update, but it
     might make cleaner draw lists */
  update(scene);
};

let run =
    (width, height, setup, createScene, draw, ~keyPressed=?, ~resize=?, ()) => {
  let canvas = Gpu.Canvas.init(width, height);
  let userState = ref(setup(canvas));
  let scene = createScene(canvas, userState^);
  calcLayout(scene);
  /* Time for resize requested, this is throttled */
  let resizeRequested = ref(None);
  let resizeThrottle = 0.7;
  /* Start render loop */
  Gl.render(
    ~window=canvas.window,
    ~displayFunc=
      f => {
        canvas.deltaTime = f /. 1000.;
        canvas.elapsed = canvas.elapsed +. canvas.deltaTime;
        if (! scene.inited) {
          /* Create updateNodes */
          queueUpdates(scene, [scene.root.id]);
          update(scene);
          if (scene.initQueued) {
            queueDrawStates(scene);
          };
          scene.inited = true;
        };
        userState := draw(userState^, scene, canvas);
        switch resizeRequested^ {
        | None => ()
        | Some(resizeTime) =>
          if (resizeTime > canvas.elapsed -. resizeThrottle) {
            resizeRequested := None;
            switch resize {
            | Some(resize) => resize(userState^)
            | None => ()
            };
            doResize(scene);
          }
        };
      },
    ~windowResize=() => resizeRequested := Some(canvas.elapsed),
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

let doAnim = (scene, anim) => scene.anims = [anim, ...scene.anims];

module SVertexObject = {
  let make = (vertexBuffer, indexBuffer) : sceneVertexObject => {
    nodes: [],
    vertexBuffer,
    indexBuffer
  };
  let makeQuad = (~usage=Gpu.StaticDraw, ()) => {
    let vBuffer = Gpu.VertexBuffer.makeQuad(~usage, ());
    let iBuffer =
      Some(Gpu.IndexBuffer.make(Gpu.IndexBuffer.makeQuadsData(1), usage));
    make(vBuffer, iBuffer);
  };
  let updateQuads = (scene, self, vertices) => {
    Gpu.VertexBuffer.setDataT(self.vertexBuffer, vertices);
    let perElement = self.vertexBuffer.perElement * 4;
    switch self.indexBuffer {
    | Some(indices) =>
      Gpu.IndexBuffer.setDataT(
        indices,
        Gpu.IndexBuffer.makeQuadsData(Array.length(vertices) / perElement)
      )
    | None => ()
    };
    queueUpdates(scene, self.nodes);
  };
};

let makeSceneUniform = uniform : sceneUniform => {nodes: [], uniform};

module UFloat = {
  let make = value => makeSceneUniform(Gpu.UniformFloat(ref(value)));
  let zero = () => makeSceneUniform(Gpu.UniformFloat(ref(0.0)));
  let one = () => makeSceneUniform(Gpu.UniformFloat(ref(1.0)));
  let set = (scene, self, v) => {
    Gpu.Uniform.setFloat(self.uniform, v);
    queueUpdates(scene, self.nodes);
  };
};

module UVec2f = {
  let zeros = () => makeSceneUniform(Gpu.UniformVec2f(ref(Data.Vec2.zeros())));
  let vals = (a, b) =>
    makeSceneUniform(Gpu.UniformVec2f(ref(Data.Vec2.make(a, b))));
  let fromArray = arr =>
    makeSceneUniform(Gpu.UniformVec2f(ref(Data.Vec2.fromArray(arr))));
  let set = (scene, self, v) => {
    Gpu.Uniform.setVec2f(self.uniform, v);
    queueUpdates(scene, self.nodes);
  };
  let setArr = (scene, self, arr) => {
    Gpu.Uniform.setVec2f(self.uniform, Data.Vec2.fromArray(arr));
    queueUpdates(scene, self.nodes);
  };
  let get = self =>
    switch self.uniform {
    | Gpu.UniformVec2f(v) => v^
    | _ => failwith("Could not get vec2 uniform")
    };
};

module UVec3f = {
  let zeros = () => makeSceneUniform(Gpu.UniformVec3f(ref(Data.Vec3.zeros())));
  let vals = (a, b, c) =>
    makeSceneUniform(Gpu.UniformVec3f(ref(Data.Vec3.make(a, b, c))));
  let fromArray = arr =>
    makeSceneUniform(Gpu.UniformVec3f(ref(Data.Vec3.fromArray(arr))));
  let vec = v => makeSceneUniform(Gpu.UniformVec3f(ref(v)));
  let set = (scene, self, v) => {
    Gpu.Uniform.setVec3f(self.uniform, v);
    queueUpdates(scene, self.nodes);
  };
  let setQuiet = (self, v) => {
    Gpu.Uniform.setVec3f(self.uniform, v);
  };
  let setArr = (scene, self, arr) => {
    Gpu.Uniform.setVec3f(self.uniform, Data.Vec3.fromArray(arr));
    queueUpdates(scene, self.nodes);
  };
  /* Sets uniforms without triggering node updates */
  let setQuiet = (self, v) => Gpu.Uniform.setVec3f(self.uniform, v);
  let get = self =>
    switch self.uniform {
    | Gpu.UniformVec3f(v) => v^
    | _ => failwith("Could not get vec3 uniform")
    };
};

module UVec4f = {
  let zeros = () => makeSceneUniform(Gpu.UniformVec4f(ref(Data.Vec4.zeros())));
  let vals = (a, b, c, d) =>
    makeSceneUniform(Gpu.UniformVec4f(ref(Data.Vec4.make(a, b, c, d))));
  let fromArray = arr =>
    makeSceneUniform(Gpu.UniformVec4f(ref(Data.Vec4.fromArray(arr))));
  let vec = v =>
    makeSceneUniform(Gpu.UniformVec4f(ref(v)));
  let set = (scene, self, v) => {
    Gpu.Uniform.setVec4f(self.uniform, v);
    queueUpdates(scene, self.nodes);
  };
  let setArr = (scene, self, arr) => {
    Gpu.Uniform.setVec4f(self.uniform, Data.Vec4.fromArray(arr));
    queueUpdates(scene, self.nodes);
  };
};

module UMat3f = {
  let id = () => makeSceneUniform(Gpu.UniformMat3f(ref(Data.Mat3.id())));
  let mat = mat => makeSceneUniform(Gpu.UniformMat3f(ref(mat)));
  let set = (scene, self, v) => {
    Gpu.Uniform.setMat3f(self.uniform, v);
    queueUpdates(scene, self.nodes);
  };
};

/* These finds the uniform in node first,
   maybe reorganize someway */
let setUniformFloat = (scene, node, key, value) => {
  let uniform = Hashtbl.find(node.uniforms, key);
  UFloat.set(scene, uniform, value);
};

let setUniformVec2f = (scene, node, key, value) => {
  let uniform = Hashtbl.find(node.uniforms, key);
  UVec2f.set(scene, uniform, value);
};

let setUniformVec3f = (scene, node, key, value) => {
  let uniform = Hashtbl.find(node.uniforms, key);
  UVec3f.set(scene, uniform, value);
};

let setUniformMat3f = (scene, node, key, value) => {
  let uniform = Hashtbl.find(node.uniforms, key);
  UMat3f.set(scene, uniform, value);
};
