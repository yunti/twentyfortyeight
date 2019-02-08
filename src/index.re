open Reprocessing;

let blockSize = 100;

let padding = 10;
let paddingAround = 20;

let puzzleSize = 4;

let backgroundColor = Utils.color(~r=184, ~g=173, ~b=162, ~a=255);

let allColors = [|
  Utils.color(~r=203, ~g=192, ~b=181, ~a=255),
  Utils.color(~r=236, ~g=228, ~b=219, ~a=255),
  Utils.color(~r=235, ~g=225, ~b=203, ~a=255),
  Utils.color(~r=232, ~g=180, ~b=130, ~a=255),
  Utils.color(~r=227, ~g=153, ~b=103, ~a=255),
  Utils.color(~r=223, ~g=119, ~b=101, ~a=255),
|];

type stateT = array(array(int));

let clone = arr => Array.map(Array.copy, arr);

let addNewElement = state => {
  let (emptyBlocks, _, _) =
    Array.fold_left(
      ((emptyBlocks, x, y), row) =>
        Array.fold_left(
          ((emptyBlocks, x, y), value) => {
            print_endline(Printf.sprintf("> (%d, %d)\n", x, y));
            if (value === 0) {
              ([(x, y), ...emptyBlocks], x + 1, y);
            } else {
              (emptyBlocks, x + 1, y);
            };
          },
          (emptyBlocks, 0, y + 1),
          row,
        ),
      ([], 0, (-1)),
      state,
    );
  if (List.length(emptyBlocks) === 0) {
    failwith("This should not happen");
  } else {
    let (x, y) = List.nth(emptyBlocks, Random.int(List.length(emptyBlocks)));
    let newState = clone(state);
    newState[y][x] = Random.int(10) === 1 ? 2 : 1;
    newState;
  };
};

let groupBy2 = row => {
  let groups =
    Array.fold_right(
      (value, acc) =>
        if (value === 0) {
          acc;
        } else {
          switch (acc) {
          | [[previous], ...rest] when previous === value => [[previous, value], ...rest]
          | _ => [[value], ...acc]
          };
        },
      row,
      [],
    );
  let size = List.length(groups);
  let rec recur = (acc, i, max) =>
    if (i >= max) {
      acc;
    } else {
      recur([[], ...acc], i + 1, max);
    };
  recur(groups, 0, puzzleSize - size);
};

let movePieces = state =>
  Array.map(
    row => {
      let groups = groupBy2(row);
      let newRow =
        List.fold_right(
          (value, acc) =>
            switch (value) {
            | [] => [0, ...acc]
            | [n] => [n, ...acc]
            | [n, _] => [n + 1, ...acc]
            | _ => failwith("This should not happen")
            },
          groups,
          [],
        );
      Array.of_list(newRow);
    },
    state,
  );

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  Array.make_matrix(puzzleSize, puzzleSize, 0);
};

let draw = (state, env) => {
  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(backgroundColor, env);
  let puzzleSizePx = (blockSize + padding) * puzzleSize - padding;
  let xOffset = (Env.width(env) - puzzleSizePx) / 2;
  let yOffset = (Env.height(env) - puzzleSizePx) / 2;
  Draw.stroke(backgroundColor, env);
  Draw.rect(
    ~pos=(xOffset - paddingAround, yOffset - paddingAround),
    ~width=puzzleSizePx + paddingAround * 2,
    ~height=puzzleSizePx + paddingAround * 2,
    env,
  );
  let newState =
    if (Env.keyPressed(Space, env)) {
      addNewElement(state);
    } else {
      state;
    };
  Array.iteri(
    (y, row) =>
      Array.iteri(
        (x, n) => {
          Draw.fill(Utils.color(~r=203, ~g=193, ~b=181, ~a=255), env);
          Draw.stroke(allColors[n], env);
          Draw.strokeWeight(5, env);
          Draw.rect(
            ~pos=(xOffset + x * (blockSize + padding), yOffset + y * (padding + blockSize)),
            ~width=blockSize,
            ~height=blockSize,
            env,
          );
        },
        row,
      ),
    newState,
  );
  newState;
};

run(~setup, ~draw, ());