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
  let (x, y) = List.nth(emptyBlocks, Random.int(List.length(emptyBlocks)));
  let newState = clone(state);
  newState[y][x] = 1;
  newState;
};

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
            ~pos=(
              xOffset + x * (blockSize + padding),
              yOffset + y * (padding + blockSize),
            ),
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