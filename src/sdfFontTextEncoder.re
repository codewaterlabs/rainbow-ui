/* From https://github.com/coolaj86/TextEncoderLite/blob/master/index.js */
type units = {mutable counter: int};

/* Helper to decrement counter and return the new counter */
let decr = (units, count) => {
  units.counter = units.counter - count;
  units.counter;
};

module ArrayBInt = ArrayB.ArrayBInt;

let utf8ToBytes = string => {
  let length = String.length(string);
  let units = {counter: length * 6};
  let leadSurrogate = ref(0);
  let codePoint = ref(0);
  let bytes = ArrayBInt.make(length + length / 8);
  /* Not sure if continue is redundant */
  let continue = ref(false);
  let break = ref(false);
  let rec processChars = i =>
    if (i < length) {
      codePoint := Char.code(string.[i]);
      continue := false;
      if (codePoint^ > 0xD7FF && codePoint^ < 0xE000) {
        /* Is surrogate component */
        if (leadSurrogate^ != 0) {
          if (codePoint^ < 0xDC00) {
            /* 2 leads in a row */
            if (decr(units, 3) > (-1)) {
              ArrayBInt.push3(bytes, 0xEF, 0xBF, 0xBD);
            };
            leadSurrogate := codePoint^;
            continue := true;
          } else {
            /* Valid surrogate pair */
            codePoint :=
              (leadSurrogate^ - 0xD800)
              lsl 10
              lor (codePoint^ - 0xDC00)
              lor 0x10000;
            leadSurrogate := 0;
          };
        } else if
          /* No lead yet */
          (codePoint^ > 0xDBFF) {
          /* Unexpected trail */
          if (decr(units, 3) > (-1)) {
            ArrayBInt.push3(bytes, 0xEF, 0xBF, 0xBD);
          };
          continue := true;
        } else if (i + 1 == length) {
          /* Unpaired lead */
          if (decr(units, 3) > (-1)) {
            ArrayBInt.push3(bytes, 0xEF, 0xBF, 0xBD);
            continue := true;
          } else {
            /* Valid lead */
            leadSurrogate := codePoint^;
            continue := true;
          };
        };
      } else if (leadSurrogate^ != 0) {
        /* Valid bmp char, but last char was a lead */
        if (decr(units, 3) > (-1)) {
          ArrayBInt.push3(bytes, 0xEF, 0xBF, 0xBD);
          leadSurrogate := 0;
        };
      };
      if (! continue^) {
        /* Encode utf-8 */
        if (codePoint^ < 0x80) {
          if (decr(units, 1) < 0) {
            break := true;
          } else {
            ArrayBInt.push(bytes, codePoint^);
          };
        } else if (codePoint^ < 0x800) {
          if (decr(units, 2) < 0) {
            break := true;
          } else {
            ArrayBInt.push2(
              bytes,
              codePoint^ asr 0x6 lor 0xC0,
              codePoint^ land 0x3F lor 0x80
            );
          };
        } else if (codePoint^ < 0x10000) {
          if (decr(units, 3) < 0) {
            break := true;
          } else {
            ArrayBInt.push3(
              bytes,
              codePoint^ asr 0xC lor 0xE0,
              codePoint^ asr 0x6 land 0x3F lor 0x80,
              codePoint^ land 0x3F lor 0x80
            );
          };
        } else if (codePoint^ < 0x200000) {
          if (decr(units, 4) < 0) {
            break := true;
          } else {
            ArrayBInt.push4(
              bytes,
              codePoint^ asr 0x12 lor 0xF0,
              codePoint^ asr 0xC land 0x3F lor 0x80,
              codePoint^ asr 0x6 land 0x3F lor 0x80,
              codePoint^ land 0x3F lor 0x80
            );
          };
        } else {
          failwith("Invalid code point");
        };
      };
      if (! break^) {
        processChars(i + 1);
      };
    };
  processChars(0);
  ArrayBInt.toArray(bytes);
};
