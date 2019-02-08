open Reprocessing;

let blockSize = 100;

let padding = 10;
let paddingAround = 20;

let puzzleSize = 4;

let backgroundColor = Utils.color(~r=184, ~g=173, ~b=162, ~a=255);

let allColors = [|Utils.color(~r=203, ~g=192, ~b=181, ~a=255)|];

type stateT = array(array(int));

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  Array.make_matrix(puzzleSize, puzzleSize, 0);
};

let draw = (state, env) => {
  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(backgroundColor, env);
  let puzzleSizePx = (blockSize + padding) * puzzleSize;
  let xOffset = (Env.width(env) - puzzleSizePx) / 2;
  let yOffset = (Env.height(env) - puzzleSizePx) / 2;
  Draw.stroke(backgroundColor, env);
  Draw.rect(
    ~pos=(xOffset - paddingAround, yOffset - paddingAround),
    ~width=puzzleSizePx + paddingAround * 2,
    ~height=puzzleSizePx + paddingAround * 2,
    env,
  );
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
    state,
  );
  state;
};

run(~setup, ~draw, ());