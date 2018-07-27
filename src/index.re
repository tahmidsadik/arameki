open Reprocessing;
let windowWidth = 640;
let windowHeight = 480;
let speed = windowWidth / 170;
let fps = 60;

type person = {x: int};

type actionType =
  | Move;

type action = {
  image: Reprocessing.imageT,
  width: int,
  height: int,
  currentPosition: (int, int),
  finalPosition: (int, int),
  duration: float,
  actionType,
  complete: bool,
  dx: float,
  dy: float,
  numberOfSteps: int,
};

type stateT = {
  spaceshipImage: Reprocessing.imageT,
  bulletImage: Reprocessing.imageT,
  position: (int, int),
  vx: int,
  vy: int,
  actions: list(action),
};

let createAction =
    (
      ~img: Reprocessing.imageT,
      ~width: int,
      ~height: int,
      ~fromPos: (int, int),
      ~toPos: (int, int),
      ~actionType: actionType,
      ~duration: float,
    )
    : action => {
  let numberOfSteps = duration *. float_of_int(fps);
  let (cx, cy) = fromPos;
  let (fx, fy) = toPos;
  let dx = float_of_int(fx - cx) /. numberOfSteps;
  let dy = float_of_int(fy - cy) /. numberOfSteps;
  {
    image: img,
    width,
    height,
    duration,
    actionType,
    complete: false,
    currentPosition: fromPos,
    finalPosition: toPos,
    dx,
    dy,
    numberOfSteps: int_of_float(numberOfSteps),
  };
};

let performAction =
    (
      {
        image,
        width,
        height,
        actionType,
        currentPosition,
        dx,
        dy,
        numberOfSteps,
      } as action,
      env,
    ) => {
  let (cx, cy) = currentPosition;
  let posx = int_of_float(float_of_int(cx) +. dx);
  let posy = int_of_float(float_of_int(cy) +. dy);
  print_string(
    "cx " ++ string_of_int(cx) ++ "\ncy: " ++ string_of_int(cy) ++ "\n",
  );
  print_string(
    "cx " ++ string_of_float(dx) ++ "\ncy: " ++ string_of_float(dy) ++ "\n",
  );
  print_string(
    "posx "
    ++ string_of_int(posx)
    ++ "\nposy: "
    ++ string_of_int(posy)
    ++ "\n",
  );
  if (numberOfSteps > 0) {
    switch (actionType) {
    | Move => Draw.image(image, ~pos=(posx, posy), ~width, ~height, env)
    };
    {
      ...action,
      currentPosition: (posx, posy),
      numberOfSteps: numberOfSteps - 1,
    };
  } else {
    {...action, complete: true};
  };
};

let setup = env : stateT => {
  Env.size(~width=windowWidth, ~height=windowHeight, env);
  {
    spaceshipImage:
      Draw.loadImage(~filename="assets/spaceship.png", ~isPixel=true, env),
    bulletImage:
      Draw.loadImage(~filename="assets/bullet.png", ~isPixel=true, env),
    position: (
      windowWidth / 2 - windowWidth / 16,
      windowHeight - (windowWidth / 8 + windowWidth / 17),
    ),
    vx: 0,
    vy: 0,
    actions: [],
  };
};

let draw = ({vx, vy, spaceshipImage, position: (x, y)} as state, env) => {
  Draw.background(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);

  Draw.image(
    spaceshipImage,
    ~pos=state.position,
    ~width=windowWidth / 8,
    ~height=windowWidth / 8,
    env,
  );
  {
    ...state,
    actions: List.map(action => performAction(action, env), state.actions),
    position: (x + vx * speed, y + - vy * speed),
  };
};

let keyPressed = ({vx, vy} as state, env) =>
  Events.(
    switch (Env.keyCode(env)) {
    | Space => {
        ...state,
        actions: [
          createAction(
            ~img=state.bulletImage,
            ~width=20,
            ~height=60,
            ~fromPos=(
              fst(state.position) + windowWidth / 18,
              snd(state.position) - 10,
            ),
            ~toPos=(fst(state.position) + windowWidth / 18, 0),
            ~actionType=Move,
            ~duration=0.5,
          ),
          ...state.actions,
        ],
      }
    | W => {...state, vy: vy + 1}
    | S => {...state, vy: vy - 1}
    | A => {...state, vx: vx - 1}
    | D => {...state, vx: vx + 1}
    | _ => state
    }
  );

let keyReleased = ({vx, vy} as state, env) =>
  Events.(
    switch (Env.keyCode(env)) {
    | W => {...state, vy: vy - 1}
    | S => {...state, vy: vy + 1}
    | A => {...state, vx: vx + 1}
    | D => {...state, vx: vx - 1}
    | _ => state
    }
  );

run(~setup, ~draw, ~keyPressed, ~keyReleased, ());