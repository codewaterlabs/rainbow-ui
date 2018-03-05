module BMFont = SdfFontBMFont;

type align =
  | Left
  | Center
  | Right;

type blockStyle = {
  align: option(align),
  scaleX: option(float),
  font: option(string),
  height: option(float),
  color: option(Color.t),
  spacing: option(float),
  lineHeight: option(float)
};

let blockStyle =
    (
      ~align=?,
      ~scaleX=?,
      ~font=?,
      ~height=?,
      ~color=?,
      ~spacing=?,
      ~lineHeight=?,
      ()
    )
    : blockStyle => {
  align,
  scaleX,
  font,
  height,
  color,
  spacing,
  lineHeight
};

type textStyle = {
  font: option(string),
  height: option(float),
  color: option(Color.t),
  spacing: option(float),
  lineHeight: option(float)
};

let textStyle =
    (~font=?, ~height=?, ~color=?, ~spacing=?, ~lineHeight=?, ())
    : textStyle => {
  font,
  height,
  color,
  spacing,
  lineHeight
};

type block = {
  style: blockStyle,
  children: list(part)
}
and styled = {
  style: textStyle,
  children: list(part)
}
and part =
  | Text(string)
  | Styled(styled)
  | Block(block);

let defaultFont = "digitalt";

let defaultHeight = 0.2;

let defaultColor = Color.fromFloats(1.0, 1.0, 1.0);

let block =
    (
      ~align=?,
      ~scaleX=?,
      ~font=?,
      ~height=?,
      ~color=?,
      ~spacing=?,
      ~lineHeight=?,
      ~style=?,
      children
    ) => {
  let style =
    switch style {
    | Some(style) => style
    | None => {align, scaleX, font, height, color, spacing, lineHeight}
    };
  Block({style, children});
};

let styled =
    (
      ~font=?,
      ~height=?,
      ~color=?,
      ~spacing=?,
      ~lineHeight=?,
      ~style=?,
      children
    ) => {
  let style =
    switch style {
    | Some(style) => style
    | None => {font, height, color, spacing, lineHeight}
    };
  Styled({style, children});
};

let text = text => Text(text);

let styledText = (~font=?, ~height=?, ~color=?, ~lineHeight=?, textString) =>
  styled(~font?, ~height?, ~color?, ~lineHeight?, [text(textString)]);

type blockInfo = {
  fonts: list(string),
  colors: list(Color.t)
};

/* Block info can be used to determine required
   fonts and colors */
let getPartInfo = (part: part) => {
  let defaultFontAdded = ref(false);
  let defaultColorAdded = ref(false);
  let collectInfo = (info, font, color) => {
    let info =
      switch font {
      | Some(font) => {...info, fonts: [font, ...info.fonts]}
      | None =>
        if (! defaultFontAdded^) {
          defaultFontAdded := true;
          {...info, fonts: [defaultFont, ...info.fonts]};
        } else {
          info;
        }
      };
    switch color {
    | Some(color) => {...info, colors: [color, ...info.colors]}
    | None =>
      if (! defaultColorAdded^) {
        defaultColorAdded := true;
        {...info, colors: [defaultColor, ...info.colors]};
      } else {
        info;
      }
    };
  };
  let rec processPart = (part, info) =>
    switch part {
    | Text(_) => info
    | Styled(styled) => processStyled(styled, info)
    | Block(block) => processBlock(block, info)
    }
  and processBlock = (block, info) => {
    let info = collectInfo(info, block.style.font, block.style.color);
    List.fold_left(
      (info, child) => processPart(child, info),
      info,
      block.children
    );
  }
  and processStyled = (styled, info) => {
    let info = collectInfo(info, styled.style.font, styled.style.color);
    List.fold_left(
      (info, child) => processPart(child, info),
      info,
      styled.children
    );
  };
  let info = processPart(part, {colors: [], fonts: []});
  {
    fonts: List.sort_uniq(String.compare, info.fonts),
    colors: List.sort_uniq(Color.compare, info.colors)
  };
};

let rec getPartText = part =>
  switch part {
  | Block(block) =>
    List.fold_left(
      (text, child) => text ++ getPartText(child),
      "",
      block.children
    )
    ++ "\n"
  | Styled(styled) =>
    List.fold_left(
      (text, child) => text ++ getPartText(child),
      "",
      styled.children
    )
  | Text(text) => text
  };

type calcStyle = {
  font: BMFont.bmFont,
  height: float,
  lineHeight: float,
  spacing: float,
  adjSpacing: float, /* Spacing adjusted with height */
  color: Color.t
};

type calcBlock = {
  lineStart: float,
  lineEnd: float,
  align,
  mutable firstLineAdded: bool,
  /* Current block x point */
  mutable xBlock: float,
  /* Start block y point
     relevant for adjecent blocks */
  mutable yBlockStart: float,
  /* Lowest block point on block line */
  mutable yBlockEnd: float
};

type layoutState = {
  /* Current glyph index */
  mutable iGlyph: int,
  /* Current x position */
  mutable penX: float,
  /* Current y position */
  mutable penY: float,
  /* Where current line ends
     Not sure if we need this now
     Was used to align whole layout,
     now we also have yBlockEnd */
  mutable yLineEnd: float,
  /* Whether a line is started, but not aligned
     and newlined in the case of a following block */
  mutable pendingLine: bool,
  /* Current line start glyph index */
  mutable glyphLineStart: int,
  /* Last glyph used to determine kerning */
  mutable lastGlyph: option(int),
  mutable curStyle: calcStyle
};

type glyphData = {
  mutable x: float,
  mutable y: float,
  mutable w: float,
  mutable h: float,
  mutable size: float,
  mutable color: array(float),
  mutable glChar: BMFont.glChar,
  mutable code: int
};

module GlyphArrayB =
  ArrayB.Make(
    {
      type a = glyphData;
      let nullChar: BMFont.glChar = {
        tx: 0.0,
        ty: 0.0,
        tx2: 0.0,
        ty2: 0.0,
        vw: 0.0,
        vh: 0.0,
        xOffset: 0.0,
        yOffset: 0.0,
        xAdvance: 0.0
      };
      /* Mutable default value with placeholder
         reference to glChar */
      let defaultVal = () : glyphData => {
        x: 0.0,
        y: 0.0,
        w: 0.0,
        h: 0.0,
        size: 0.0,
        color: [|1.0, 1.0, 1.0|],
        glChar: nullChar,
        code: 0
      };
      let isStaticVal = false;
    }
  );

module FontLayout = {
  /* Globally usable data for layout */
  type t = {
    store: FontStore.t,
    glyphB: GlyphArrayB.t
  };
  let make = (store: FontStore.t) : t => {
    store,
    glyphB: GlyphArrayB.make(100)
  };
  let numVertices = 20;
  let numColorVertices = 32;
  let getVertices = (layout, numGlyphs, multicolor) => {
    let glyphB = layout.glyphB.data;
    /* Four points per glyph, each point has
       x, y, uvx, uvy. Multicolor has 3 more per point
       for color */
    let perGlyph = multicolor ? numColorVertices : numVertices;
    let d = Array.make(numGlyphs * perGlyph, 0.0);
    let rec processGlyph = (iGlyph, i) =>
      if (iGlyph < numGlyphs) {
        let g = glyphB[iGlyph];
        let gl = g.glChar;
        /* Bottom left */
        d[i] = g.x;
        d[i + 1] = g.y -. g.h;
        d[i + 2] = gl.tx;
        d[i + 3] = gl.ty2;
        d[i + 4] = g.size;
        /* Bottom right */
        d[i + 5] = g.x +. g.w;
        d[i + 6] = g.y -. g.h;
        d[i + 7] = gl.tx2;
        d[i + 8] = gl.ty2;
        d[i + 9] = g.size;
        /* Top right */
        d[i + 10] = g.x +. g.w;
        d[i + 11] = g.y;
        d[i + 12] = gl.tx2;
        d[i + 13] = gl.ty;
        d[i + 14] = g.size;
        /* Top left */
        d[i + 15] = g.x;
        d[i + 16] = g.y;
        d[i + 17] = gl.tx;
        d[i + 18] = gl.ty;
        d[i + 19] = g.size;
        /* Recurse next glyph */
        processGlyph(iGlyph + 1, i + perGlyph);
      };
    let rec processColorGlyph = (iGlyph, i) =>
      if (iGlyph < numGlyphs) {
        let g = glyphB[iGlyph];
        let gl = g.glChar;
        /* Bottom left */
        d[i] = g.x;
        d[i + 1] = g.y -. g.h;
        d[i + 2] = gl.tx;
        d[i + 3] = gl.ty2;
        d[i + 4] = g.size;
        d[i + 5] = g.color[0];
        d[i + 6] = g.color[1];
        d[i + 7] = g.color[2];
        /* Bottom right */
        d[i + 8] = g.x +. g.w;
        d[i + 9] = g.y -. g.h;
        d[i + 10] = gl.tx2;
        d[i + 11] = gl.ty2;
        d[i + 12] = g.size;
        d[i + 13] = g.color[0];
        d[i + 14] = g.color[1];
        d[i + 15] = g.color[2];
        /* Top right */
        d[i + 16] = g.x +. g.w;
        d[i + 17] = g.y;
        d[i + 18] = gl.tx2;
        d[i + 19] = gl.ty;
        d[i + 20] = g.size;
        d[i + 21] = g.color[0];
        d[i + 22] = g.color[1];
        d[i + 23] = g.color[2];
        /* Top left */
        d[i + 24] = g.x;
        d[i + 25] = g.y;
        d[i + 26] = gl.tx;
        d[i + 27] = gl.ty;
        d[i + 28] = g.size;
        d[i + 29] = g.color[0];
        d[i + 30] = g.color[1];
        d[i + 31] = g.color[2];
        /* Recurse next glyph */
        processColorGlyph(iGlyph + 1, i + perGlyph);
      };
    if (multicolor) {
      processColorGlyph(0, 0);
    } else {
      processGlyph(0, 0);
    };
    d;
  };
  let defaultStyle = layout => {
    let font = Hashtbl.find(layout.store.fonts, defaultFont);
    {
      font,
      height: defaultHeight,
      lineHeight: 2.0,
      spacing: 0.0,
      adjSpacing: 0.0,
      color: defaultColor
    };
  };
  let defaultBlock = () => {
    lineStart: (-1.0),
    lineEnd: 1.0,
    firstLineAdded: false,
    align: Left,
    xBlock: (-1.0),
    yBlockStart: 0.0,
    yBlockEnd: 0.0
  };
  /* Todo: Verify algorithms when we have better setup
     to "unit test"
     Would it be good for performance to split this up? */
  /* This is a bit complex and is going to be somewhat
     messy and performance should be important,
     but todo: more clarity would be welcome */
  let layoutPart = (layout, part: part) => {
    let curStyle = defaultStyle(layout);
    let rootBlock = defaultBlock();
    /* State while doing layout */
    /* Todo: Use font baseline */
    let s = {
      iGlyph: 0,
      penX: rootBlock.lineStart,
      penY: 0.0,
      yLineEnd: 0.0,
      pendingLine: false,
      lastGlyph: None,
      glyphLineStart: 0,
      curStyle
    };
    /* Some linespacing factor, lineHeight might be off somehow */
    let glyphB = layout.glyphB;
    let spaceCode = Char.code(' ');
    let nlCode = Char.code('\n');
    /* Move characters in range in x direction */
    let rec moveX = (delta, i, until) => {
      glyphB.data[i].x = glyphB.data[i].x +. delta;
      if (i < until) {
        moveX(delta, i + 1, until);
      };
    };
    /* Move characters in range in x and y direction */
    let rec moveXY = (deltaX, deltaY, i, until) => {
      glyphB.data[i].x = glyphB.data[i].x +. deltaX;
      glyphB.data[i].y = glyphB.data[i].y +. deltaY;
      if (i <= until) {
        moveXY(deltaX, deltaY, i + 1, until);
      };
    };
    let alignLine = (lastIndex, curBlock) =>
      /* If there is a newline as first, char,
         this will be called with lastIndex = -1 */
      if (lastIndex >= 0) {
        let lastGlyph = glyphB.data[lastIndex];
        switch curBlock.align {
        | Left => ()
        | Center =>
          let xAdj = (curBlock.lineEnd -. lastGlyph.x -. lastGlyph.w) /. 2.0;
          moveX(xAdj, s.glyphLineStart, lastIndex);
        | Right =>
          let xAdj = curBlock.lineEnd -. lastGlyph.x -. lastGlyph.w;
          moveX(xAdj, s.glyphLineStart, lastIndex);
        };
      };
    /* First non whitespace index from i and backwards */
    let rec nonWhitespace = (i, until) =>
      if (i < until) {
        None;
      } else if (glyphB.data[i].code == spaceCode) {
        nonWhitespace(i - 1, until);
      } else {
        Some(i);
      };
    let printBuffer = () => {
      let rec loop = i =>
        if (i < s.iGlyph) {
          Js.log(String.make(1, Char.chr(glyphB.data[i].code)));
          loop(i + 1);
        };
      loop(0);
    };
    let truncateWhitespace = fromIndex => {
      /* Truncate between nonWhitespace and fromIndex
         by moving glyphs backwards and placing the
         truncated glyphs after current iGlyph */
      let rec collectTruncated = (i, until, list) =>
        if (i < until) {
          collectTruncated(i + 1, until, [glyphB.data[i], ...list]);
        } else {
          list;
        };
      let rec moveBack = (i, until, truncateNum) =>
        if (i <= until) {
          glyphB.data[i] = glyphB.data[i + truncateNum];
          moveBack(i + 1, until, truncateNum);
        };
      let truncateFrom =
        switch (nonWhitespace(fromIndex, s.glyphLineStart)) {
        | Some(nonWhitespace) => nonWhitespace + 1
        | None => s.glyphLineStart
        };
      let numTruncated = fromIndex - truncateFrom + 1;
      let numAfter = s.iGlyph - 1 - fromIndex;
      let numMove = min(numTruncated, numAfter);
      if (numMove > 0) {
        let truncated =
          collectTruncated(truncateFrom, truncateFrom + numMove, []);
        moveBack(truncateFrom, truncateFrom + numMove, numTruncated);
        /* Put truncated items where items where moved from */
        List.iteri(
          (i, truncated) =>
            glyphB.data[truncateFrom + numTruncated + i] = truncated,
          truncated
        );
      };
      numTruncated;
    };
    let rec prevSpace = i =>
      if (i <= s.glyphLineStart) {
        None;
      } else if (glyphB.data[i].code == spaceCode) {
        Some(i);
      } else {
        prevSpace(i - 1);
      };
    /* Since adding a line may be done on last
       whitespace, iGlyph and penX are not changed here.
       Also todo line height should in that case
       be influenced by required height of moved text
       could handle this maybe by looking it s.iGlyph */
    let addLine = (lastIndex, curStyle, curBlock) => {
      alignLine(lastIndex, curBlock);
      s.glyphLineStart = lastIndex + 1;
      s.lastGlyph = None;
      let lineDist = curStyle.height *. curStyle.lineHeight;
      /* Go to next line */
      s.penY = s.penY -. lineDist;
      s.yLineEnd = s.yLineEnd -. lineDist;
      /* Child blocks should start after line */
      curBlock.yBlockStart = s.yLineEnd;
      if (s.yLineEnd < curBlock.yBlockEnd) {
        curBlock.yBlockEnd = s.yLineEnd;
      };
    };
    let addFirstLine = (curStyle, curBlock) => {
      let lineDist = curStyle.height *. curStyle.lineHeight;
      /* Todo: Added 2.0 factor to base altough not sure about it,
         was lineHeight, figure out what is appropriate */
      s.penY =
        s.penY
        -. curStyle.height
        +. curStyle.font.common.glBase
        *. curStyle.height
        *. 2.0;
      s.yLineEnd = s.yLineEnd -. lineDist;
      /* Child blocks should start after line */
      curBlock.yBlockStart = s.yLineEnd;
      if (s.yLineEnd < curBlock.yBlockEnd) {
        curBlock.yBlockEnd = s.yLineEnd;
      };
      s.glyphLineStart = s.iGlyph;
      s.lastGlyph = None;
    };
    /* Loops parts and add result to glyphBuffer */
    /* Lots of dangerous mutable code and bad microoptimizations
       The state and glyph buffer can be shared between
       layout calls to avoid allocations. One could potentially
       create own layout object maybe to better support
       cases like editable text or other text that changes */
    let rec layoutParts = (parts, curStyle, curBlock) =>
      switch parts {
      | [] => ()
      | [part, ...rest] =>
        switch part {
        | Text(text) =>
          if (! curBlock.firstLineAdded) {
            curBlock.firstLineAdded = true;
            addFirstLine(curStyle, curBlock);
          };
          s.pendingLine = true;
          let textLen = String.length(text);
          GlyphArrayB.ensureSize(layout.glyphB, s.iGlyph + textLen);
          let glScale = curStyle.font.common.glScale;
          /* Loop for character in text */
          let rec addChar = iText =>
            if (iText < textLen) {
              let code = Char.code(text.[iText]);
              /* Check for newline char */
              if (code == nlCode) {
                /* Move back to first non whitespace */
                switch (nonWhitespace(s.iGlyph - 1, s.glyphLineStart)) {
                | Some(iGlyph) =>
                  s.iGlyph = iGlyph + 1;
                  addLine(iGlyph, curStyle, curBlock);
                  s.penX = curBlock.lineStart;
                  addChar(iText + 1);
                | None =>
                  /* No non-whitespace found.. Assume the user wants a newline */
                  addLine(s.iGlyph - 1, curStyle, curBlock);
                  s.penX = curBlock.lineStart;
                  addChar(iText + 1);
                };
              } else {
                /* Normal character */
                let glChar =
                  switch curStyle.font.glChars[code] {
                  | Some(glChar) => glChar
                  | None => BMFont.addGlChar(curStyle.font, code)
                  };
                /* Kerning and spacing */
                switch s.lastGlyph {
                | Some(lastGlyph) =>
                  /* Kerning is not scaled to gl */
                  s.penX =
                    s.penX
                    +. curStyle.adjSpacing
                    +. float_of_int(
                         BMFont.getKerning(curStyle.font, lastGlyph, code)
                       )
                    *. glScale
                    *. curStyle.height
                | None => s.penX = s.penX +. curStyle.adjSpacing
                };
                /* Check if we surpass max width */
                let iText =
                  if (s.penX +. glChar.vw *. curStyle.height > curBlock.lineEnd) {
                    /* Skip whitespaces from text position when start of line */
                    let rec firstNonWhitespace = iText =>
                      if (iText >= textLen) {
                        None;
                      } else if (text.[iText] == ' ') {
                        firstNonWhitespace(iText + 1);
                      } else {
                        Some(iText);
                      };
                    /* Next char surpassed line width, attempt to find
                       whitespace to break on, or break after current char */
                    switch (prevSpace(s.iGlyph - 1)) {
                    | Some(spaceIndex) =>
                      /* Especially for right aligned, we need to truncate whitespace
                         Will also reduce vertex data */
                      let truncated = truncateWhitespace(spaceIndex);
                      /* Hm, why not iGlyph - truncated */
                      s.iGlyph = s.iGlyph - truncated + 1;
                      let spaceIndex = spaceIndex - truncated;
                      /* Spaceindex - 1? */
                      addLine(spaceIndex, curStyle, curBlock);
                      if (s.iGlyph - 1 > spaceIndex) {
                        /* There are characters after space */
                        /* Move characters after to next line */
                        /* todo: Could use spaceIndex + 2.x if available */
                        let subtractX =
                          (glyphB.data[spaceIndex + 1].x +. 1.0) *. (-1.0);
                        /* todo: Need to resolve current height for next line
                           based on moved characters */
                        let addY =
                          curStyle.height *. curStyle.lineHeight *. (-1.0);
                        moveXY(subtractX, addY, spaceIndex + 1, s.iGlyph - 1);
                        let lastChar = glyphB.data[s.iGlyph - 1];
                        s.penX =
                          lastChar.x
                          +. lastChar.glChar.xAdvance
                          *. curStyle.height;
                      } else {
                        s.penX = curBlock.lineStart;
                      };
                      firstNonWhitespace(iText);
                    | None =>
                      /* No space found, break after current char */
                      addLine(s.iGlyph - 1, curStyle, curBlock);
                      switch (firstNonWhitespace(iText)) {
                      | Some(_) as iText =>
                        s.penX = curBlock.lineStart;
                        iText;
                      | None => None
                      };
                    };
                  } else {
                    /* Within line, add as normal */
                    Some(iText);
                  };
                /* There may have been a break and then
                   no more non-whitespace */
                switch iText {
                | Some(iText) =>
                  /* Set glyph data */
                  let glyph = glyphB.data[s.iGlyph];
                  glyph.x = s.penX +. glChar.xOffset *. curStyle.height;
                  glyph.y = s.penY -. glChar.yOffset *. curStyle.height;
                  glyph.w = glChar.vw *. curStyle.height;
                  glyph.h = glChar.vh *. curStyle.height;
                  glyph.size = curStyle.height;
                  glyph.code = code;
                  glyph.color = Color.toArray(curStyle.color);
                  glyph.glChar = glChar;
                  /* Recurse rest of chars in text */
                  if (iText < textLen) {
                    s.iGlyph = s.iGlyph + 1;
                    /* Increment posX */
                    s.penX = s.penX +. glChar.xAdvance *. curStyle.height;
                    addChar(iText + 1);
                  };
                | None => ()
                };
              };
            };
          /* Initiate char loop */
          addChar(0);
        | Styled(styled) =>
          let style = styled.style;
          let height =
            switch style.height {
            | None => curStyle.height
            | Some(height) => height
            };
          let spacing =
            switch style.spacing {
            | None => curStyle.spacing
            | Some(spacing) => spacing
            };
          let adjSpacing = spacing *. height;
          let font =
            switch style.font {
            | None => curStyle.font
            | Some(font) => Hashtbl.find(layout.store.fonts, font)
            };
          let newStyle = {
            font,
            height,
            lineHeight:
              switch style.lineHeight {
              | None => curStyle.lineHeight
              | Some(lineHeight) => lineHeight
              },
            spacing,
            adjSpacing,
            color:
              switch style.color {
              | None => curStyle.color
              | Some(color) => color
              }
          };
          s.curStyle = newStyle;
          layoutParts(styled.children, newStyle, curBlock);
        | Block(block) =>
          let style = block.style;
          if (s.pendingLine) {
            s.pendingLine = false;
            alignLine(s.iGlyph - 1, curBlock);
          };
          /* Set curStyle from block */
          let height =
            switch style.height {
            | None => curStyle.height
            | Some(height) => height
            };
          let spacing =
            switch style.spacing {
            | None => curStyle.spacing
            | Some(spacing) => spacing
            };
          let adjSpacing = spacing *. height;
          let font =
            switch style.font {
            | None => curStyle.font
            | Some(font) => Hashtbl.find(layout.store.fonts, font)
            };
          let curStyle = {
            font,
            height,
            lineHeight:
              switch style.lineHeight {
              | None => curStyle.lineHeight
              | Some(lineHeight) => lineHeight
              },
            spacing,
            adjSpacing,
            color:
              switch style.color {
              | None => curStyle.color
              | Some(color) => color
              }
          };
          let curBlockWidth = curBlock.lineEnd -. curBlock.lineStart;
          /* Scale is relative to parent block, then there is also
             xBlock point from adjecent blocks where block
             continues from */
          let blockWidth =
            switch style.scaleX {
            | None => curBlockWidth
            | Some(scaleX) => curBlockWidth *. scaleX
            };
          /* Check if block fits on same line or begin new line */
          let (lineStart, lineEnd) =
            if (curBlock.xBlock +. blockWidth <= curBlock.lineEnd) {
              /* Fits on same block line */
              let lineStart = curBlock.xBlock;
              curBlock.xBlock = curBlock.xBlock +. blockWidth;
              (lineStart, lineStart +. blockWidth);
            } else {
              /* Start new block line */
              curBlock.yBlockStart = curBlock.yBlockEnd;
              curBlock.xBlock = curBlock.lineStart +. blockWidth;
              (curBlock.lineStart, curBlock.lineStart +. blockWidth);
            };
          let childBlock = {
            lineStart,
            lineEnd,
            firstLineAdded: false,
            align:
              switch style.align {
              | None => curBlock.align
              | Some(align) => align
              },
            xBlock: curBlock.xBlock,
            yBlockStart: curBlock.yBlockStart,
            yBlockEnd: curBlock.yBlockStart
          };
          s.curStyle = curStyle;
          /* Todo: Need to take baseline into consideration more places,
             when changing height/fonts. Some edge cases around newlines,
             how to handle based on previous lines font/height */
          /* Set pen to block start coords */
          s.penX = lineStart;
          s.penY = curBlock.yBlockStart;
          s.yLineEnd = s.penY;
          layoutParts(block.children, curStyle, childBlock);
          /* Align last line in block if pending */
          if (s.pendingLine) {
            s.pendingLine = false;
            alignLine(s.iGlyph - 1, childBlock);
          };
          /* Adjust block end by child blocks */
          if (childBlock.yBlockEnd < curBlock.yBlockEnd) {
            curBlock.yBlockEnd = childBlock.yBlockEnd;
          };
        };
        /* Recurse rest of part list */
        layoutParts(rest, curStyle, curBlock);
      };
    layoutParts([part], curStyle, rootBlock);
    (s.iGlyph, rootBlock.yBlockEnd);
  };
  let layoutVertices = (layout, part, multicolor) => {
    let (iGlyph, yLineEnd) = layoutPart(layout, part);
    (getVertices(layout, iGlyph, multicolor), yLineEnd);
  };
};
