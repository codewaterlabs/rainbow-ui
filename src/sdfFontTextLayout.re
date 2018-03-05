/* https://github.com/Jam3/layout-bmfont-text */
module BMFont = SdfFontBMFont;

type align =
  | AlignLeft
  | AlignRight
  | AlignCenter;

type layoutCoords = {
  x: int,
  y: int
};

type layoutGlyph = {
  position: layoutCoords,
  data: BMFont.bmChar,
  index: int,
  line: int
};

type textLayout = {
  text: string,
  textLen: int,
  font: BMFont.bmFont,
  width: int,
  align,
  letterSpacing: int,
  tabSize: int,
  above: bool,
  flipY: bool
};

let make =
    (
      text,
      font,
      width,
      ~align=AlignLeft,
      ~letterSpacing=0,
      ~tabSize=4,
      ~above=false,
      ~flipY=false,
      ()
    ) => {
  let textLen = String.length(text);
  /* Mistaken code to strip char */
  /*
   let chr10 = Char.chr(10);
   let (textLen, text) = if (String.contains(text, chr10)) {
     let rec stripped = (index) => {
       if (String.contains_from(text, index, chr10)) {
         let nextPos = String.index_from(text, index, chr10);
         String.sub(text, index, nextPos - index) ++ stripped(nextPos + 1)
       } else {
         String.sub(text, index, textLen - index);
       }
     };
     let str = stripped(0);
     (String.length(str), str)
   } else {
     (textLen, text)
   };
   */
  {text, textLen, font, width, align, letterSpacing, tabSize, above, flipY};
};

type fittingChars = {
  start: int,
  last: int,
  width: int
};

type measureState = {
  mutable curPen: int,
  mutable curWidth: int,
  mutable count: int,
  mutable lastGlyph: option(BMFont.bmChar)
};

/* Get chars fitting on a line */
let measureFittingChars = (self, start, last, width) => {
  let d = {curPen: 0, curWidth: 0, count: 0, lastGlyph: None};
  let calcEnd = self.textLen < last ? self.textLen : last;
  let rec untilLimit = i => {
    let id = Char.code(self.text.[i]);
    if (id != 10) {
      let glyph =
        switch self.font.charsById[id] {
        | Some(bmChar) => bmChar
        | None => failwith("Char not found")
        };
      let kerning =
        switch d.lastGlyph {
        | Some(lastGlyph) =>
          BMFont.getKerning(self.font, lastGlyph.id, glyph.id)
        | None => 0
        };
      d.curPen = d.curPen + kerning;
      let nextPen = d.curPen + glyph.xAdvance + self.letterSpacing;
      let nextWidth = d.curPen + glyph.width;
      if (nextWidth < width && nextPen < width) {
        d.curPen = nextPen;
        d.curWidth = nextWidth;
        d.lastGlyph = Some(glyph);
        d.count = d.count + 1;
        if (i + 1 < calcEnd) {
          untilLimit(i + 1);
        };
      };
    };
  };
  untilLimit(start);
  switch d.lastGlyph {
  | Some(lastGlyph) => d.curWidth = d.curWidth + lastGlyph.xOffset
  | None => ()
  };
  {start, last: start + d.count, width: d.curWidth};
};

/**
 * Wraps text to lines and returns indexes
 * and width for each line
 */
let lineWraps = (self, width, start, last) => {
  let textLength = self.textLen;
  let curStart = ref(start);
  /* Return index of next newline,
     wrapped in option. None for not found */
  let rec optionNChar = i =>
    if (i >= textLength - 1) {
      None;
    } else if (self.text.[i] == '\n') {
      Some(i);
    } else {
      optionNChar(i + 1);
    };
  let rec calcLines = lines =>
    if (curStart^ >= last || curStart^ >= textLength) {
      lines;
    } else {
      let nextNChar = optionNChar(curStart^);
      let nextNewline =
        switch nextNChar {
        | Some(nextNewline) => nextNewline
        | None => textLength
        };
      /* Skip whitespaces at the start of line */
      while (curStart^ < nextNewline && self.text.[curStart^] == ' ') {
        curStart := curStart^ + 1;
      };
      /* Get chars fitting on current line */
      let {start: fitStart, last: fitEnd, width: _fitWidth} =
        measureFittingChars(self, curStart^, last, width);
      let lineEnd = ref(curStart^ + (fitEnd - fitStart));
      let nextStart = ref(lineEnd^ + 1);
      /* If limit was reached before newline */
      if (lineEnd^ < nextNewline) {
        /* Attempt to find space to break on */
        while (lineEnd^ > curStart^ && self.text.[lineEnd^] != ' ') {
          lineEnd := lineEnd^ - 1;
        };
        if (lineEnd^ == curStart^) {
          /* We went all the way to start without whitespace */
          if (nextStart^ > curStart^ + 1) {
            nextStart := nextStart^ - 1;
          };
          lineEnd := nextStart^;
        } else {
          /* We found space to break on */
          nextStart := lineEnd^;
          /* Strip whitespace from end of line */
          while (lineEnd^ > curStart^ && self.text.[lineEnd^ - 1] == ' ') {
            lineEnd := lineEnd^ - 1;
          };
        };
      };
      let calcedStart = curStart^;
      curStart := nextStart^;
      if (lineEnd^ >= calcedStart) {
        calcLines([
          measureFittingChars(self, calcedStart, lineEnd^, width),
          ...lines
        ]);
      } else {
        /* is this right/neccesary? */
        calcLines(lines);
      };
    };
  List.rev(calcLines([]));
};

/* Pen position */
type penPos = {
  mutable x: int,
  mutable y: int
};

let update = (self: textLayout) => {
  let lines = lineWraps(self, self.width, 0, self.textLen);
  let maxLineWidth =
    List.fold_left(
      (max, fitting) => fitting.width > max ? fitting.width : max,
      0,
      lines
    );
  let penPos = {x: 0, y: 0};
  let lineHeight = self.font.common.lineHeight;
  let baseline = self.font.common.base;
  let descender = lineHeight - baseline;
  let height = lineHeight * List.length(lines) - descender;
  let (_, glyphs) =
    List.fold_left(
      ((lineIndex, glyphs), line) => {
        let lastGlyph = ref(None);
        let rec forChar = (charIndex, glyphs) =>
          if (charIndex >= line.last) {
            glyphs;
          } else {
            let id = Char.code(self.text.[charIndex]);
            let bmChar =
              switch self.font.charsById[id] {
              | Some(bmChar) => bmChar
              | None => failwith("Could not find char")
              };
            switch lastGlyph^ {
            | Some((lastGlyph: BMFont.bmChar)) =>
              penPos.x =
                penPos.x
                + BMFont.getKerning(self.font, lastGlyph.id, bmChar.id)
            | None => ()
            };
            let tx =
              switch self.align {
              | AlignCenter => penPos.x + (maxLineWidth - line.width) / 2
              | AlignRight => penPos.x + (maxLineWidth - line.width)
              | AlignLeft => penPos.x
              };
            let glyph = {
              position: {
                x: tx,
                y: penPos.y
              },
              data: bmChar,
              index: charIndex,
              line: lineIndex
            };
            penPos.x = penPos.x + bmChar.xAdvance + self.letterSpacing;
            lastGlyph := Some(bmChar);
            forChar(charIndex + 1, [glyph, ...glyphs]);
          };
        let lineGlyphs = forChar(line.start, []);
        penPos.y = penPos.y - lineHeight;
        penPos.x = 0;
        (lineIndex + 1, [List.rev(lineGlyphs), ...glyphs]);
      },
      (0, []),
      lines
    );
  (maxLineWidth, List.concat(List.rev(glyphs)));
};

let vertexData = (self, glyphs) => {
  let data = Array.make(List.length(glyphs) * 4 * 2 * 2, 0.0);
  let texWidth = float_of_int(self.font.common.scaleW);
  let texHeight = float_of_int(self.font.common.scaleH);
  List.iteri(
    (i, glyph) => {
      /* Bottom left position */
      let x = float_of_int(glyph.position.x + glyph.data.xOffset);
      let y = float_of_int(glyph.position.y - glyph.data.yOffset);
      /* Quad size */
      let w = float_of_int(glyph.data.width);
      let h = float_of_int(glyph.data.height);
      /* Uv data */
      let bitmap = glyph.data;
      let bw = float_of_int(bitmap.x + bitmap.width);
      let bh = float_of_int(bitmap.y + bitmap.height);
      /* Top left positions */
      let u0 = float_of_int(bitmap.x) /. texWidth;
      let u1 = bw /. texWidth;
      let v0 = float_of_int(bitmap.y) /. texHeight;
      let v1 = bh /. texHeight;
      /* Register positions x, y, and uvs */
      /* Bottom left */
      let idx = i * 16;
      data[idx] = x;
      data[idx + 1] = y -. h;
      data[idx + 2] = u0;
      data[idx + 3] = v1;
      /* Bottom right */
      data[idx + 4] = x +. w;
      data[idx + 5] = y -. h;
      data[idx + 6] = u1;
      data[idx + 7] = v1;
      /* Top right */
      data[idx + 8] = x +. w;
      data[idx + 9] = y;
      data[idx + 10] = u1;
      data[idx + 11] = v0;
      /* Top left */
      data[idx + 12] = x;
      data[idx + 13] = y;
      data[idx + 14] = u0;
      data[idx + 15] = v0;
    },
    glyphs
  );
  data;
};

/* todo: to floats */
let positions = (self, glyphs) => {
  let data = Array.make(List.length(glyphs) * 4 * 2, 0);
  List.iteri(
    (i, glyph) => {
      /* Bottom left position */
      let x = glyph.position.x + glyph.data.xOffset;
      let y = glyph.position.y;
      /* Quad size */
      let w = glyph.data.width;
      let h = glyph.data.height;
      let idx = i * 8;
      /* Bottom left */
      data[idx] = x;
      data[idx + 1] = y;
      /* Top left */
      data[idx + 2] = x;
      data[idx + 3] = y + h;
      /* Top right */
      data[idx + 4] = x + w;
      data[idx + 5] = y + h;
      /* Bottom right */
      data[idx + 6] = x + w;
      data[idx + 7] = y;
    },
    glyphs
  );
  data;
};

let uvs = (self, glyphs) => {
  let data = Array.make(List.length(glyphs) * 4 * 2, 0);
  let texWidth = self.font.common.scaleW;
  let texHeight = self.font.common.scaleH;
  List.iteri(
    (i, glyph) => {
      let bitmap = glyph.data;
      let bw = bitmap.x + bitmap.width;
      let bh = bitmap.y + bitmap.height;
      /* Top left positions */
      let u0 = bitmap.x / texWidth;
      let u1 = bw / texWidth;
      let v1 =
        self.flipY ? (texHeight - bitmap.y) / texHeight : bitmap.y / texHeight;
      let v0 = self.flipY ? (texHeight - bh) / texHeight : bh / texHeight;
      /* Register positions x, y, and uvs */
      /* Bottom left */
      let idx = i * 8;
      data[idx] = u0;
      data[idx + 1] = v1;
      /* Top left */
      data[idx + 2] = u0;
      data[idx + 3] = v0;
      /* Top right */
      data[idx + 4] = u1;
      data[idx + 5] = v0;
      /* Bottom right */
      data[idx + 6] = u1;
      data[idx + 7] = v1;
    },
    glyphs
  );
  data;
};
