/* Used to support hashing different variations of a
   program and storing it in a Hashtbl, then loading
   or creating based on some progType */
module type StoreSpec = {
  type hash;
  type progType;
  let getHash: progType => hash;
  let createProgram: progType => Scene.sceneProgram;
  let tblSize: int;
};

module Make = (StoreSpec: StoreSpec) => {
  let programs: Hashtbl.t(StoreSpec.hash, Scene.sceneProgram) =
    Hashtbl.create(StoreSpec.tblSize);
  let getProgram = (progType: StoreSpec.progType) => {
    let hash = StoreSpec.getHash(progType);
    if (! Hashtbl.mem(programs, hash)) {
      let program = StoreSpec.createProgram(progType);
      Hashtbl.add(programs, hash, program);
      program;
    } else {
      Hashtbl.find(programs, hash);
    };
  };
};
