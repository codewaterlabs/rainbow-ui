let listToTbl = list => {
  let listLen = List.length(list);
  let tbl = Hashtbl.create(listLen > 0 ? listLen : 1);
  List.iter(((key, item)) => Hashtbl.add(tbl, key, item), list);
  tbl;
};

let listRange = countDown => {
  let rec addToList = (list, countDown) =>
    if (countDown <= 0) {
      list;
    } else {
      addToList([countDown, ...list], countDown - 1);
    };
  addToList([], countDown);
};

let isSome = (o: option('a)) =>
  switch o {
  | Some(_) => true
  | None => false
  };

module Timer = {
  type timed = {
    start: int,
    mutable ended: option(int)
  };
  type performance;
  let perf: performance = [%bs.raw "performance"];
  [@bs.send] external performanceNow : performance => int = "now";
  let start = () => {start: performanceNow(perf), ended: None};
  let endPrint = (~label=?, timed) => {
    let cTime = performanceNow(perf);
    let time = cTime - timed.start;
    switch label {
    | Some(label) => Printf.printf("Time %s %d milliseconds\n", label, time)
    | None => Printf.printf("Time %d milliseconds\n", time)
    };
  };
};
