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
  Utils.color(~r=223, ~g=129, ~b=101, ~a=255),
  Utils.color(~r=246, ~g=94, ~b=59, ~a=255),
  Utils.color(~r=237, ~g=207, ~b=114, ~a=255),
  Utils.color(~r=237, ~g=204, ~b=97, ~a=255),
  Utils.color(~r=237, ~g=200, ~b=80, ~a=255),
  Utils.color(~r=237, ~g=197, ~b=63, ~a=255),
  Utils.color(~r=237, ~g=194, ~b=46, ~a=255),
  Utils.color(~r=60, ~g=58, ~b=50, ~a=255),
|];

type stateT = {
  grid: array(array(int)),
  font: fontT,
  fontSmall: fontT,
};

let clone = arr => Array.map(Array.copy, arr);

let addNewElement = state => {
  let (emptyBlocks, _, _) =
    Array.fold_left(
      ((emptyBlocks, _, y), row) =>
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
  };
  let (x, y) = List.nth(emptyBlocks, Random.int(List.length(emptyBlocks)));
  let newState = clone(state);
  newState[y][x] = Random.int(10) === 1 ? 2 : 1;
  newState;
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

let rotateGridRight = grid => {
  let newGrid = Array.make_matrix(puzzleSize, puzzleSize, 0);
  for (y in 0 to puzzleSize - 1) {
    for (x in 0 to puzzleSize - 1) {
      newGrid[y][x] = grid[puzzleSize - 1 - x][y];
    };
  };
  newGrid;
};

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  {
    grid: Array.make_matrix(puzzleSize, puzzleSize, 0),
    font: Draw.loadFont(~filename="assets/font.fnt", env),
    fontSmall: Draw.loadFont(~filename="assets/fontSmall.fnt", env),
  };
};

let draw = ({grid, font, fontSmall} as state, env) => {
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
  let newGrid =
    if (Env.keyPressed(Space, env)) {
      addNewElement(grid);
    } else {
      grid;
    };
  let newGrid =
    if (Env.keyPressed(A, env)) {
      rotateGridRight(newGrid);
    } else {
      newGrid;
    };
  let newGrid =
    if (Env.keyPressed(Right, env)) {
      movePieces(newGrid);
    } else {
      newGrid;
    };
  let newGrid =
    if (Env.keyPressed(Up, env)) {
      newGrid
      |> rotateGridRight
      |> movePieces
      |> rotateGridRight
      |> rotateGridRight
      |> rotateGridRight;
    } else {
      newGrid;
    };
  let newGrid =
    if (Env.keyPressed(Left, env)) {
      newGrid
      |> rotateGridRight
      |> rotateGridRight
      |> movePieces
      |> rotateGridRight
      |> rotateGridRight;
    } else {
      newGrid;
    };
  let newGrid =
    if (Env.keyPressed(Down, env)) {
      newGrid
      |> rotateGridRight
      |> rotateGridRight
      |> rotateGridRight
      |> movePieces
      |> rotateGridRight;
    } else {
      newGrid;
    };
  Array.iteri(
    (y, row) =>
      Array.iteri(
        (x, n) => {
          Draw.fill(allColors[n], env);
          Draw.stroke(allColors[n], env);
          Draw.strokeWeight(5, env);
          Draw.rect(
            ~pos=(xOffset + x * (blockSize + padding), yOffset + y * (padding + blockSize)),
            ~width=blockSize,
            ~height=blockSize,
            env,
          );
          switch (n) {
          | 0 => ()
          | 1
          | 2
          | 3 =>
            Draw.text(
              ~font,
              ~body=string_of_int(Utils.pow(~base=2, ~exp=n)),
              ~pos=(
                xOffset + x * (blockSize + padding) + 28,
                yOffset + y * (padding + blockSize) + 20,
              ),
              env,
            )
          | 4
          | 5
          | 6 =>
            Draw.text(
              ~font,
              ~body=string_of_int(Utils.pow(~base=2, ~exp=n)),
              ~pos=(
                xOffset + x * (blockSize + padding) + 6,
                yOffset + y * (padding + blockSize) + 20,
              ),
              env,
            )
          | _ =>
            Draw.text(
              ~font,
              ~body=string_of_int(Utils.pow(~base=2, ~exp=n)),
              ~pos=(
                xOffset + x * (blockSize + padding) + 28,
                yOffset + y * (padding + blockSize) + 36,
              ),
              env,
            )
          };
        },
        row,
      ),
    newGrid,
  );
  {...state, grid: newGrid};
};

run(~setup, ~draw, ());