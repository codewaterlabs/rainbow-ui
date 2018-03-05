/* Buf data could be TypedArray behind the scenes
   (as is the case with xhr arraybuffer request)
   Generated code should work.. but also
   investigate ways to get bracket syntax
   for typedarray
   */
/* https://github.com/beatgammit/base64-js/blob/master/index.js */
let fromBase64 = base64 => {
  let len = Array.length(base64);
  if (len mod 4 > 0) {
    failwith("Invalid base64 string.");
  };
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  let lookup = Array.make(String.length(chars), ' ');
  let maxCode = ref(0);
  String.iteri(
    (i, char) => {
      lookup[i] = char;
      let code = Char.code(char);
      if (code > maxCode^) {
        maxCode := code;
      };
    },
    chars
  );
  let dashCode = Char.code('-');
  let underscoreCode = Char.code('_');
  /* Not sure if these are needed */
  if (dashCode > maxCode^) {
    maxCode := dashCode;
  };
  if (underscoreCode > maxCode^) {
    maxCode := underscoreCode;
  };
  let revLookup = Array.make(maxCode^, 0);
  String.iteri(
    (i, char) => {
      let code = Char.code(char);
      revLookup[code] = i;
    },
    chars
  );
  revLookup[dashCode] = 62;
  revLookup[underscoreCode] = 63;
  /* There might be placeholders on the end of the string "=" */
  let equalCode = Char.code('=');
  let placeholders =
    if (base64[len - 2] == equalCode) {
      2;
    } else if (base64[len - 1] == equalCode) {
      1;
    } else {
      0;
    };
  /* Could be uint8/byte array */
  let bin = Array.make(len * 3 / 4 - placeholders, 0);
  /* If there are placeholders, only get up to the last complete 4 chars */
  let l = placeholders > 0 ? len - 4 : len;
  let rec loop = (i, j) => {
    let tmp =
      revLookup[base64[i]]
      lsl 18
      lor revLookup[base64[i + 1]]
      lsl 12
      lor revLookup[base64[i + 2]]
      lsl 6
      lor revLookup[base64[i + 3]];
    bin[j] = tmp asr 16 land 0xFF;
    bin[j + 1] = tmp asr 8 land 0xFF;
    bin[j + 2] = tmp land 0xFF;
    if (i + 4 < l) {
      loop(i + 4, j + 3);
    } else {
      (i, j);
    };
  };
  let (i, j) = loop(0, 0);
  if (placeholders == 2) {
    let tmp = revLookup[base64[i]] lsl 2 lor revLookup[base64[i + 1]] asr 4;
    bin[j] = tmp land 0xFF;
  } else if (placeholders == 1) {
    let tmp =
      revLookup[base64[i]]
      lsl 10
      lor revLookup[base64[i + 1]]
      lsl 4
      lor revLookup[base64[i + 2]]
      asr 2;
    bin[j] = tmp asr 8 land 0xFF;
    bin[j + 1] = tmp land 0xFF;
  };
  bin;
};

let readUInt32LE = (bytes, offset) =>
  bytes[offset]
  lor bytes[offset + 1]
  lsl 8
  lor bytes[offset + 2]
  lsl 16
  + bytes[offset + 3]
  * 0x1000000;

let readInt32LE = (bytes, offset) =>
  bytes[offset]
  lor bytes[offset + 1]
  lsl 8
  lor bytes[offset + 2]
  lsl 16
  lor bytes[offset + 3]
  lsl 24;

let readUInt16LE = (bytes, offset) =>
  bytes[offset] lor bytes[offset + 1] lsl 8;

let readInt16LE = (bytes, offset) => {
  let read = bytes[offset] lor bytes[offset + 1] lsl 8;
  if (read land 0x8000 == 0) {
    read;
  } else {
    read lor 0xFFFF0000;
  };
};

let readUInt8 = (bytes, offset) => bytes[offset];

let readInt8 = (bytes, offset) => {
  let read = bytes[offset];
  if (read land 0x80 == 0) {
    read;
  } else {
    (0xFF - read + 1) * (-1);
  };
};

let utf8Slice = (bytes, start, last) => {
  let rec getCodePoints = (i, codePoints) =>
    if (i >= last) {
      codePoints;
    } else {
      let firstByte = bytes[i];
      let codePoint = ref(-1);
      let bytesPerSequence =
        if (firstByte > 0xEF) {
          4;
        } else if (firstByte > 0xDF) {
          3;
        } else if (firstByte > 0xBF) {
          2;
        } else {
          1;
        };
      if (i + bytesPerSequence <= last) {
        switch bytesPerSequence {
        | 1 =>
          if (firstByte < 0x80) {
            codePoint := firstByte;
          }
        | 2 =>
          let secondByte = bytes[i + 1];
          if (secondByte land 0xC0 == 0x80) {
            let tempCodePoint =
              (firstByte land 0x1F) lsl 0x6 lor (secondByte land 0x3F);
            if (tempCodePoint > 0x7F) {
              codePoint := tempCodePoint;
            };
          };
        | 3 =>
          let secondByte = bytes[i + 1];
          let thirdByte = bytes[i + 2];
          if (secondByte land 0xC0 == 0x80 && thirdByte land 0xC0 == 0x80) {
            let tempCodePoint =
              (firstByte land 0xF)
              lsl 0xC
              lor (secondByte land 0x3F)
              lsl 0x6
              lor (thirdByte land 0x3F);
            if (tempCodePoint > 0x7FF
                && (tempCodePoint < 0xD800 || tempCodePoint > 0xDFFF)) {
              codePoint := tempCodePoint;
            };
          };
        | 4 =>
          let secondByte = bytes[i + 1];
          let thirdByte = bytes[i + 2];
          let fourthByte = bytes[i + 3];
          if (secondByte
              land 0xC0 == 0x80
              && thirdByte
              land 0xC == 0x80
              && fourthByte
              land 0xC0 == 0x80) {
            let tempCodePoint =
              (firstByte land 0xF)
              lsl 0x12
              lor (secondByte land 0x3F)
              lsl 0xC
              lor (thirdByte land 0x3F)
              lsl 0x6
              lor (fourthByte land 0x3F);
            if (tempCodePoint > 0xFFFF && tempCodePoint < 0x110000) {
              codePoint := tempCodePoint;
            };
          };
        | _ => ()
        };
      };
      if (codePoint^ == (-1)) {
        /* No valid codepoint generated
           Insert replacement char */
        codePoint := 0xFFFD;
        getCodePoints(i + 1, [codePoint^, ...codePoints]);
      } else {
        /* Encode to utf16 (surrogate dance) */
        /* todo: Seems this should use the first
           codepoint when codepoint < 0x80 */
        codePoint := codePoint^ - 0x10000;
        let codePoint1 = codePoint^ lsr 10 land 0x3FF lor 0xD800;
        let codePoint2 = 0xDC00 lor codePoint^ land 0x3FF;
        getCodePoints(
          i + bytesPerSequence,
          [codePoint1, codePoint2, ...codePoints]
        );
      };
    };
  /* todo: from unicode code */
  List.fold_right(
    (code, str) => str ++ String.make(1, Char.chr(code)),
    getCodePoints(start, []),
    ""
  );
};

let readStringSlice = (bytes, offset, last) => {
  let rec stringFromCodes = (str, i) => {
    let chr = String.make(1, Char.chr(bytes[i]));
    if (i + 1 < last) {
      stringFromCodes(str ++ chr, i + 1);
    } else {
      str ++ chr;
    };
  };
  stringFromCodes("", offset);
};

let readStringNT = (bytes, offset) => {
  let pos = ref(offset);
  let len = Array.length(bytes);
  while (pos^ < len && bytes[pos^] != 0x00) {
    pos := pos^ + 1;
  };
  /*utf8Slice(bytes, offset, pos^)*/
  /* Todo: decode from utf8. */
  readStringSlice(bytes, offset, pos^);
};
