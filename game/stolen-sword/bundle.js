(function () {
  'use strict';

  let _id = 0;
  const key = () => _id++;

  const KEY_ENEMY_IS_DEFENCING = key();
  const KEY_ENEMY_IS_UNTOUCHABLE = key();
  const KEY_ENEMY_DEAD_FRAME = key();
  const KEY_ENEMY_IS_DEAD = key();
  const KEY_ENEMY_HEALTH = key();
  const KEY_ENEMY_LAST_DAMAGE_FRAME = key();
  const KEY_ENEMY_APPEARANCE = key();
  const KEY_ENEMY_COMPUND_PARENT = key();

  const KEY_STAGE_INITIATE = key();
  const KEY_STAGE_IS_WAVE_CLEAN = key();
  const KEY_STAGE_WAVES = key();
  const KEY_STAGE_TRANSITION = key();
  const KEY_STAGE_TRANSITION_FRAME = key();
  const KEY_STAGE_ENDING_CUT_SCENE = key();
  const KEY_STAGE_ENDING_CUT_SCENE_FRAME = key();
  const KEY_STAGE_ENDING_CUT_SCENE_INDEX = key();
  const KEY_STAGE_ENDING_CUT_SCENE_KEY = key();
  const KEY_STAGE_START_KEY = key();
  const KEY_GAME_START_KEY = key();

  const KEY_OBJECT_ON_UPDATE = key();
  const KEY_OBJECT_FRAME = key();
  const KEY_OBJECT_INITIAL_POS = key();
  const KEY_OBJECT_ON_COLLIDED = key();
  const KEY_OBJECT_IS_COLLIDED = key();
  const KEY_OBJECT_FORCE_CHECK_COLLISION = key();
  const KEY_OBJECT_EVENT_GET_OFFSET = key();
  const KEY_OBJECT_EVENT_IS_REPEAT = key();
  const KEY_OBJECT_EVENT_LAST_TRIGGER_FRAME = key();
  const KEY_OBJECT_EVENT_FIRST_FRAME_TRIGGER = key();
  const KEY_OBJECT_Z_INDEX = key();

  const KEY_PLAYER_DAMAGE_FRAME = key();
  const KEY_PLAYER_DEATH_FRAME = key();
  const KEY_PLAYER_ATTACK_FRAME = key();
  const KEY_PLAYER_STOP_FRAME = key();
  const KEY_PLAYER_CHARGE_FRAME = key();

  const KEY_PROJECTILE_IS_COMSUMED = key();
  const KEY_PROJECTILE_SORUCE = key();

  const KEY_GRAPHIC_IS_ANIMATION_FINISH = key();

  const KEY_SAVE_NEED_TUTORIAL = key();
  const KEY_SAVE_STAGE = key();
  const KEY_SAVE_WAVE = key();

  const SIDE_T = 't';
  const SIDE_R = 'r';
  const SIDE_B = 'b';
  const SIDE_L = 'l';

  const ASPECT_RATIO = 16 / 11;
  const DEFAULT_FRAME_HEIGHT = 667;
  const DEFAULT_FRAME_WIDTH = DEFAULT_FRAME_HEIGHT / ASPECT_RATIO;
  const SLOW_DOWN_DURATION = 3000;
  const SLOW_MOTION_TIME_RATIO = 0.05;
  const NORAML_TIME_RATIO = 1;
  const FRAME_DURAITON = 16;

  const G = 0.5;
  const GROUND_FRICTION = 0.2;
  const WALL_FRICTION = 0.3;

  const MAX_RELEASE_VELOCITY = 14;
  const DRAG_FORCE_FACTOR = 10;
  const DEFAULT_DASH = 2;
  const MINIMUM_DASH_VELOCITY = 2;
  const TRAJECTORY_LINE_LENGTH = 200;
  const DEFAULT_HEALTH = 2;

  const ENEMY_DEATH_ANIMATION_DURATION = 1000;
  const PLAYER_DAMAGE_INVINCIBLE_DURAION = 1000;
  const PLAYER_DEATH_ANIMATION_DURATION = 1000;
  // export const STAGE_TRANSITION_DURAION = 100;
  const STAGE_TRANSITION_DURAION = 2000;

  const POSE_RUN = [0.109, 0.021, 0.08, -0.13, 0.119, 0.051, 0, 0.148, -0.968];
  const POSE_CHARGE = [0.08, 0, -0.056, -0.068, -0.073, -0.002, 0.231, 0.091, 0.078];
  const POSE_IDLE = [0,0,0,0,0,0,0,0,-1.025];
  const POSE_STOP =  [0.025, 0, -0.109, -0.085, 0.027, -0.027, 0.107, -0.073, -1.062];
  const POSE_ATTACK = [0.069, 0.025, 0.068, -0.235, -0.172, -0.514, 0.066, 0.089, 0.424];
  const POSE_DAMAGED = [-0.072, -0.089, -0.138, -0.148, -0.225, -0.144, -0.054, 0.043, -1.186];
  const POSE_DIE = [-0.234, -0.275, -0.237, -0.235, -0.218, -0.18, -0.154, -0.183, -1.462];
  const POSE_SWIM = [0.107, 0.01, 0.114, 0.119, -0.26, -0.247, 0.104, 0.215, -0.057];


  /**
   *  zIndex
   *  0-10: background
   * 11-20: enemy
   * 21-30: player
   * 31-40: platform
   * 41-50: effect
   * 51-60: foreground
   * 61-70: menu/hud
   */

  const approach = (value, target, step) => {
    step = Math.abs(step);
    const diff = Math.abs(value - target);
    const sign = (value - target) / (diff || 1);
    return diff > step ? value - step * sign : target;
  };
  const radiansToDegrees = (radians) => (radians * 180) / Math.PI;
  const vector = (x, y) => ({ x, y });
  const vectorAngle = (vectorA, vectorB) =>
    Math.atan2(vectorB.y - vectorA.y, vectorB.x - vectorA.x);
  const vectorOp = (callback, vectors, output = {}) => {
    output.x = callback(...vectors.map(({ x }) => x));
    output.y = callback(...vectors.map(({ y }) => y));
    return output;
  };
  const vectorDistance = (vectorA, vectorB) =>
    Math.hypot(vectorA.x - vectorB.x, vectorA.y - vectorB.y);
  const vectorMagnitude = (vectorA) =>
    vectorDistance(vectorA, vector(0, 0));
  const intersection = (a, b, c, d) => {
    const uA =
      ((d.x - c.x) * (a.y - c.y) - (d.y - c.y) * (a.x - c.x)) /
      ((d.y - c.y) * (b.x - a.x) - (d.x - c.x) * (b.y - a.y));
    const uB =
      ((b.x - a.x) * (a.y - c.y) - (b.y - a.y) * (a.x - c.x)) /
      ((d.y - c.y) * (b.x - a.x) - (d.x - c.x) * (b.y - a.y));

    if (uA >= 0 && uA <= 1 && uB >= 0 && uB <= 1) {
      return vector(a.x + uA * (b.x - a.x), a.y + uA * (b.y - a.y));
    }
  };

  const object = (x, y, w, h, vx = 0, vy = 0) => ({
    p: vector(x, y),
    s: vector(w, h),
    v: vector(vx, vy),
    [KEY_OBJECT_FRAME]: 0,
    [KEY_OBJECT_INITIAL_POS]: vector(x, y),
  });

  const getObjectBoundary = ({ p, s }) => ({
    [SIDE_L]: p.x - s.x / 2,
    [SIDE_T]: p.y + s.y / 2,
    [SIDE_R]: p.x + s.x / 2,
    [SIDE_B]: p.y - s.y / 2,
  });

  const isOverlap = (objectA, objectB, timeRatio) => {
    const boundaryA = getObjectBoundary(objectA);
    const boundaryB = getObjectBoundary(objectB);
    return (
      boundaryA.l + objectA.v.x * timeRatio <
        boundaryB.r + objectB.v.x * timeRatio &&
      boundaryA.r + objectA.v.x * timeRatio >
        boundaryB.l + objectB.v.x * timeRatio &&
      boundaryA.t + objectA.v.y * timeRatio >
        boundaryB.b + objectB.v.y * timeRatio &&
      boundaryA.b + objectA.v.y * timeRatio <
        boundaryB.t + objectB.v.y * timeRatio
    );
  };

  const SIDES = [
    [SIDE_L, SIDE_T, SIDE_R, SIDE_T],
    [SIDE_R, SIDE_T, SIDE_R, SIDE_B],
    [SIDE_R, SIDE_B, SIDE_L, SIDE_B],
    [SIDE_L, SIDE_B, SIDE_L, SIDE_T],
  ];
  const isGoingThrough = (objectA, objectB, timeRatio) => {
    const nextAPos = vectorOp((pos, v) => pos + v * timeRatio, [
      objectA.p,
      objectA.v,
    ]);
    const boundaryB = getObjectBoundary(objectB);
    for (let [x1, y1, x2, y2] of SIDES) {
      const isIntersected = intersection(
        objectA.p,
        nextAPos,
        vector(boundaryB[x1], boundaryB[y1]),
        vector(boundaryB[x2], boundaryB[y2])
      );
      if (isIntersected) return true;
    }
    return false;
  };

  const getClosetSide = (objectA, objectB) => {
    const angle = radiansToDegrees(vectorAngle(objectB.p, objectA.p));
    const boundaryB = getObjectBoundary(objectB);
    const ltAngle = radiansToDegrees(
      vectorAngle(objectB.p, vector(boundaryB.l, boundaryB.t))
    );
    const rtAngle = 180 - ltAngle;
    if (angle > rtAngle && angle < ltAngle) return SIDE_T;
    if (angle > ltAngle || angle < -ltAngle) return SIDE_L;
    if (angle < -rtAngle && angle > -ltAngle) return SIDE_B;
    if (angle > -rtAngle || angle < -rtAngle) return SIDE_R;
  };

  const getActionProgress = (frame, duration, repeat = true) =>
    (repeat ? frame % Math.round(duration / FRAME_DURAITON) : frame) /
    Math.round(duration / FRAME_DURAITON);

  const alternateProgress = (process) => Math.abs(process - 0.5) * 2;

  const objectAction = (interval, callback, options = {}) => object => {
    let frame = object[KEY_OBJECT_FRAME];
    if (options[KEY_OBJECT_EVENT_GET_OFFSET])
      frame -= options[KEY_OBJECT_EVENT_GET_OFFSET](object) || frame;
    if(frame > 0) callback(object, getActionProgress(frame, interval));
  };
    
  const objectEvent = (callback, interval, options = {}) => {
    const lastTriggerFrameKey = KEY_OBJECT_EVENT_LAST_TRIGGER_FRAME + key();
    return (object) => {
      if (
        options[KEY_OBJECT_EVENT_IS_REPEAT] !== false ||
        !object[lastTriggerFrameKey]
      ) {
        const targetFrame = Math.round(interval / FRAME_DURAITON);
        let frame = object[KEY_OBJECT_FRAME];
        if (options[KEY_OBJECT_EVENT_GET_OFFSET])
          frame -= options[KEY_OBJECT_EVENT_GET_OFFSET](object) || frame;
        frame = Math.round(frame);
        if (
          (options[KEY_OBJECT_EVENT_FIRST_FRAME_TRIGGER] || frame > 0) &&
          frame !== object[lastTriggerFrameKey] &&
          frame % targetFrame === 0
        ) {
          callback(object);
          object[lastTriggerFrameKey] = Math.round(object[KEY_OBJECT_FRAME]);
        }
      }
    };
  };

  function decompressPath(str, offsetX = 0, offsetY = 0, scale = 1) {
    let z = 'charCodeAt';
    let x = 0;
    let y = 0;
    let xMin = 0;
    let yMin = 0;
    let xMax = 0;
    let yMax = 0;
    const result = [];
    str.split('').map(i => {
      let j = i[z]();
      let a = -(j >> 3) * 0.39 + 4.72;
      let d = (j & 7) * 4 + 4;
      x += d * Math.cos(a);
      y -= d * Math.sin(a);
      xMin = Math.min(x, xMin);
      yMin = Math.min(y, yMin);
      xMax = Math.max(x, xMax);
      yMax = Math.max(y, yMax);
      result.push(vector(x, y));
    });
    result.forEach(p => {
      p.x -= (xMax - xMin) / 2 + offsetX;
      p.y -= (yMax - yMin) / 2 + offsetY;
      p.x *= scale;
      p.y *= scale;
    });
    return {
      p: result.splice(1, result.length),
      w: xMax - xMin,
      h: yMax - yMin
    }
  }

  const lerp = (a, b, p) => a + (b - a) * p;

  const rotate = (center, pos, angle) => {
    const cos = Math.cos(angle * 2 * Math.PI);
    const sin = Math.sin(angle * 2 * Math.PI);
    return vector(
      (cos * (pos.x - center.x)) + (sin * (pos.y - center.y)) + center.x,
      (cos * (pos.y - center.y)) - (sin * (pos.x - center.x)) + center.y
    )
  };

  const easeInQuad = t => t * t;
  const easeOutQuad = t => t * (2 - t);
  const easeInOutQuad = t => (t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t);
  const easeInQuint = t => t * t * t * t * t;
  const easeOutQuint = t => 1 + --t * t * t * t * t;
  const easeInOutQuint = t => (t < 0.5 ? 16 * t * t * t * t * t : 1 + 16 * --t * t * t * t * t);
  const easeInCirc = t => -(Math.sqrt(1 - t * t) - 1);
  const easeOutCirc = t => Math.sqrt(1 - Math.pow(t - 1, 2));

  let zzfxV=.3;    // volume
  let zzfx=       // play sound
  (q=1,k=.05,c=220,e=0,t=0,u=.1,r=0,F=1,v=0,z=0,w=0,A=0,l=0,B=0,x=0,G=0,d=0,y=1,m=0,C=0)=>{let b=2*Math.PI,H=v*=500*b/zzfxR**2,I=(0<x?1:-1)*b/4,D=c*=(1+2*k*Math.random()-k)*b/zzfxR,Z=[],g=0,E=0,a=0,n=1,J=0,K=0,f=0,p,h;e=99+zzfxR*e;m*=zzfxR;t*=zzfxR;u*=zzfxR;d*=zzfxR;z*=500*b/zzfxR**3;x*=b/zzfxR;w*=b/zzfxR;A*=zzfxR;l=zzfxR*l|0;for(h=e+m+t+u+d|0;a<h;Z[a++]=f)++K%(100*G|0)||(f=r?1<r?2<r?3<r?Math.sin((g%b)**3):Math.max(Math.min(Math.tan(g),1),-1):1-(2*g/b%2+2)%2:1-4*Math.abs(Math.round(g/b)-g/b):Math.sin(g),f=(l?1-C+C*Math.sin(2*Math.PI*a/l):1)*(0<f?1:-1)*Math.abs(f)**F*q*zzfxV*(a<e?a/e:a<e+m?1-(a-e)/m*(1-y):a<e+m+t?y:a<h-d?(h-a-d)/u*y:0),f=d?f/2+(d>a?0:(a<h-d?1:(h-a)/d)*Z[a-d|0]/2):f),p=(c+=v+=z)*Math.sin(E*x-I),g+=p-p*B*(1-1E9*(Math.sin(a)+1)%2),E+=p-p*B*(1-1E9*(Math.sin(a)**2+1)%2),n&&++n>A&&(c+=w,D+=w,n=0),!l||++J%l||(c=D,v=H,n=n||1);q=zzfxX.createBuffer(1,h,zzfxR);q.getChannelData(0).set(Z);c=zzfxX.createBufferSource();c.buffer=q;c.connect(zzfxX.destination);c.start();return c};
  let zzfxX=new(window.AudioContext||webkitAudioContext); // audio context
  let zzfxR=22050; // sample rate

  const sounds = [
    [,0,1e3,.01,.08,.09,4,,-20.1,8.4,,,,1,,,,.51,.02], //slash
    [1.5,.5,90,,,1,1,1.91,,,,,,5,50,3], //damage
    [,,1313,,.08,.16,3,.04,,,-23,.09,,.4,,.05,,.21,.13,.05], // bounce
    [,,1774,.03,,.06,4,.01,,,-697,.03,,5,6,,.13,.18,.01,.5], // jump
    [,0,260,,,1,,.6], //harmony
  ];

  const soundtime = [];

  const playHarmony = () => {
    const now = Date.now();
    if(!soundtime[4] || (now - soundtime[4] > 200)) {
      soundtime[4] = now;
      [130.8, 164.8, 220, 261.6, 329.6, 440].forEach(p => {
        sounds[4][2] = p;
        zzfx(...sounds[4]);
      });
    }
  };

  const playSound = (index, interval = 200) => {
    const now = Date.now();
    if(!soundtime[index] || now - soundtime[index] > interval) {
      zzfx(...sounds[index]);
      soundtime[index] = now;
    }
  };

  function resumeAudio() {
    zzfxX.resume();
  }

  const ref = (defaultValue) =>
    new Proxy(
      { 0: defaultValue },
      {
        get: (object) => object[0],
        set: (object, p, value) => {
          object[0] = value;
          return true;
        },
      }
    );

  // Player
  const player = object(0, 0, 20, 50);
  const $health = ref(DEFAULT_HEALTH);
  const $dash = ref(DEFAULT_DASH);
  const $trajectoryLineOpacity = ref(0);
  const $g = ref(G);
  const $maxReleaseVelocity = ref(MAX_RELEASE_VELOCITY);
  const $playerCollisionSide = ref({});
  const $forceFacing = ref();

  function getReleaseVelocity() {
    const v = vector(
      (pressDownPos.x - cursorPos.x) / DRAG_FORCE_FACTOR,
      (cursorPos.y - pressDownPos.y) / DRAG_FORCE_FACTOR
    );
    const vm = vectorMagnitude(v);
    if (vm > $maxReleaseVelocity.$)
      vectorOp((v) => (v * $maxReleaseVelocity.$) / vm, [v], v);
    return v;
  }
  function playerTrajectory() {
    const path = [vectorOp((p) => p, [player.p])];
    const estimateV = getReleaseVelocity();
    let distance = 0;
    while (distance < TRAJECTORY_LINE_LENGTH) {
      const lastP = path[path.length - 1];
      const nextP = vectorOp((pos, v) => pos + v, [lastP, estimateV]);
      distance += vectorDistance(lastP, nextP);
      estimateV.y -= $g.$;
      path.push(nextP);
    }
    return path;
  }
  function dash() {
    if (isAbleToDash() && isReleaseVelocityEnough()) {
      const v = getReleaseVelocity();
      player.v.x = v.x;
      player.v.y = v.y;
      $dash.$--;
      playSound(3, 0);
    }
  }
  function playerDamage() {
    if (!isPlayerInvincibleAfterDamage()) {
      if ($health.$ > 0) {
        player.v = vector((-1 * player.v.x) / Math.abs(player.v.x || 1), 5);
      }
      if ($health.$ > 1) {
        playSound(1);
        player[KEY_PLAYER_DAMAGE_FRAME] = player[KEY_OBJECT_FRAME];
        setDash(Math.max($dash.$, 1));
      }
      $health.$ = Math.max(0, $health.$ - 1);
    }
  }
  function revive() {
    $health.$ = DEFAULT_HEALTH;
    player[KEY_PLAYER_DEATH_FRAME] = undefined;
  }

  function resetDash() {
    setDash(DEFAULT_DASH);
  }
  function setDash(value) {
    if ($isPressing.$ && $dash.$ !== value) slowDown();
    $dash.$ = value;
  }
  function isAbleToDash() {
    return (
      $dash.$ > 0 &&
      !player[KEY_PLAYER_DEATH_FRAME] &&
      $stage.$ &&
      $stageWave.$ !== -1 &&
      !$stage.$[KEY_STAGE_TRANSITION_FRAME] &&
      !isInTranisition()
    );
  }
  function isReleaseVelocityEnough() {
    return vectorMagnitude(getReleaseVelocity()) >= MINIMUM_DASH_VELOCITY;
  }
  function isPlayerInvincibleAfterDamage() {
    return (
      player[KEY_PLAYER_DAMAGE_FRAME] &&
      player[KEY_OBJECT_FRAME] - player[KEY_PLAYER_DAMAGE_FRAME] <=
        PLAYER_DAMAGE_INVINCIBLE_DURAION / FRAME_DURAITON
    );
  }

  // Interaction
  const $isPressing = ref(false);
  const cursorPos = vector(0, 0);
  const pressDownPos = vector(0, 0);

  // Camera
  const $cameraLoop = ref();
  const cameraCenter = vector(0, 0);
  const cameraFrameSize = vector(window.innerWidth, window.innerHeight);
  const $cameraZoom = ref(1);

  const getReflection = object => {
    if($reflectionY.$ !== undefined && object.p.y > $reflectionY.$) {
      const [x, y] = reflect(object.p, $reflectionY.$);
      return { x: x - Math.random() * 4 * $timeRatio.$, y, d: easeInQuad(Math.min(1, (object.p.y - $reflectionY.$) / object.s.y * 2)) }
    }
  };

  const isUnderWater = object => $reflectionGradient.$ && getObjectBoundary(object).b <= $reflectionY.$;

  function reflect(value, y, ratio = 1) {
    const scale = cameraFrameSize.y / DEFAULT_FRAME_HEIGHT;
    return [
      cameraFrameSize.x / 2 -
        (cameraCenter.x - value.x) * $cameraZoom.$ * scale * ratio,
      cameraFrameSize.y / 2 + 
        (cameraCenter.y + value.y - y) * $cameraZoom.$ * scale * ratio
    ];
  }

  function transform(value, ratio = 1) {
    const scale = cameraFrameSize.y / DEFAULT_FRAME_HEIGHT;
    if (typeof value === 'number') {
      return value * $cameraZoom.$ * scale * ratio;
    } else {
      return [
        cameraFrameSize.x / 2 -
          (cameraCenter.x - value.x) * $cameraZoom.$ * scale * ratio,
        cameraFrameSize.y / 2 +
          (cameraCenter.y - value.y) * $cameraZoom.$ * scale * ratio,
      ];
    }
  }

  function isOutOfScreen(object) {
    const canvasPos = transform(object.p);
    return (
      canvasPos[0] < 0 ||
      canvasPos[1] < 0 ||
      canvasPos[0] > cameraFrameSize.x ||
      canvasPos[1] > cameraFrameSize.y
    );
  }

  const collision = (objectA, objectB) => {
    if (
      (objectB[KEY_OBJECT_FORCE_CHECK_COLLISION] || !isOutOfScreen(objectB)) &&
      (isOverlap(objectA, objectB, $timeRatio.$) || isGoingThrough(objectA, objectB, $timeRatio.$)))
      return getClosetSide(objectA, objectB);
  };

  // Time
  const $timeRatio = ref(NORAML_TIME_RATIO);
  const animations = [];
  let animationId = 0;
  let cancelTimeRatioAnimation;

  function removeAnimation(id) {
    const index = animations.findIndex(([_id]) => _id === id);
    if (index !== -1) animations.splice(index, 1);
  }

  function animateTo(callback, duration = 1, timingFunc = (v) => v) {
    let frame = 0;
    return stepTo(
      () =>
        callback(
          timingFunc(
            Math.min(Math.max((frame++ * FRAME_DURAITON) / duration, 0), 1)
          )
        ),
      () => frame * FRAME_DURAITON > duration
    );
  }

  function stepTo(callback, shouldStop) {
    const id = animationId++;
    animations.push([id, callback, shouldStop]);
    return () => removeAnimation(id);
  }

  function slowDown(ratio = SLOW_MOTION_TIME_RATIO) {
    if (!cancelTimeRatioAnimation) {
      cancelTimeRatioAnimation = animateTo(
        (progress) => {
          $timeRatio.$ =
            NORAML_TIME_RATIO -
            (NORAML_TIME_RATIO - ratio) * easeOutQuint(progress);
        },
        SLOW_DOWN_DURATION,
        easeOutQuint
      );
    }
  }

  function backToNormal() {
    if (cancelTimeRatioAnimation) {
      cancelTimeRatioAnimation();
      cancelTimeRatioAnimation = undefined;
    }
    $timeRatio.$ = NORAML_TIME_RATIO;
  }

  // Stage
  const $isGameStarted = ref(false);
  const enemies = [];
  const projectiles = [];
  const platforms = [];
  const graphics = [];
  const effects = [];
  const $stageWave = ref(-1);
  const $stageNextWave = ref(-1);
  const $stageIndex = ref(-1);
  const $stage = ref();
  const $backgroundV = ref(0);
  const $backgroundColor = ref();
  const isInTranisition = () =>
    $stage.$ &&
    ($stageWave.$ === $stage.$[KEY_STAGE_WAVES].length ||
      $stage.$[KEY_STAGE_TRANSITION_FRAME] !== undefined);

  const $debug = ref(false);
  let clickPromises = {};
  const resolveClick = () => {
    Object.keys(clickPromises).forEach((key) => clickPromises[key]());
    clickPromises = {};
  };
  const waitForClick = (key, callback) => (clickPromises[key] = callback);

  const drawStack = [];
  const draw = (zIndex, callback) =>
    drawStack[zIndex]
      ? drawStack[zIndex].push(callback)
      : (drawStack[zIndex] = [callback]);

  const $reflectionY = ref();
  const $reflectionGradient = ref();

  const offscreenCtx = document.createElement('canvas').getContext('2d');
  const createLinearGradient = (y, h, colors, distance, depth) => {
    const grad = offscreenCtx.createLinearGradient(...transform(vector(0, y), distance), ...transform(vector(0, y - h), depth ? distance : undefined));
    colors.forEach(color => grad.addColorStop(...color));
    return grad;
  };

  const $theft = ref();
  const $tempPlayerPos = ref();
  const $canvasLeftOffset = ref(0);

  const save = (...args) => window.localStorage.setItem(...args);
  const load = (...args) => window.localStorage.getItem(...args);

  const needTutorial = load(KEY_SAVE_NEED_TUTORIAL) != 1;
  const $titleY = ref();

  const getOffset = (startTime) =>
    startTime ? () => startTime / FRAME_DURAITON : undefined;

  const circular = (x, y, rx, ry, progress, ratio = 1) => vector(
    x + rx * Math.cos(progress * 2 * Math.PI) * ratio,
    y + ry * Math.sin(progress * 2 * Math.PI) * ratio
  );
    
  const circularMovement = (duration, xRadius, yRadius, startTime = 0, getProgress = v => v) => {
    let radiusProgress = 0;
    return objectAction(
      duration,
      (object, progress) => {
        if(!object[KEY_ENEMY_DEAD_FRAME]) {
          radiusProgress = Math.max(progress, radiusProgress);
          vectorOp(pos => pos, [
            circular(
              object[KEY_OBJECT_INITIAL_POS].x,
              object[KEY_OBJECT_INITIAL_POS].y,
              xRadius,
              yRadius,
              getProgress(progress),
              radiusProgress
            )
          ], object.p);
        }
      },
      {
        [KEY_OBJECT_EVENT_GET_OFFSET]: getOffset(startTime),
      }
    );
  };

  const slideIn = (duration, x, y, timingFunc = easeOutQuint) =>
    objectAction(duration, (object, progress) => {
      if (!object[KEY_ENEMY_DEAD_FRAME] && progress > 0 && object[KEY_OBJECT_FRAME] < duration / FRAME_DURAITON) {
        vectorOp(
          (to, from) => from + (to - from) * timingFunc(progress),
          [object[KEY_OBJECT_INITIAL_POS], vector(x, y)],
          object.p
        );
      }
    });
    
  const follow = (object, offset, startTime) => 
    enemy => {
      if(!enemy[KEY_ENEMY_DEAD_FRAME] && enemy[KEY_OBJECT_FRAME] > startTime / FRAME_DURAITON) {
        enemy.p.y = object.p.y + offset.y;
        enemy.p.x = object.p.x + offset.x;
      }
    };
    
  const chase = (head, durations) => {
    const maxDuration = Math.max(...durations);
    const path = [];
    let lastFrame = 0;
    head[KEY_OBJECT_ON_UPDATE].push(enemy => {
      if(enemy[KEY_OBJECT_FRAME] > 0 && lastFrame !== Math.floor(enemy[KEY_OBJECT_FRAME])) {
        path.unshift(vector(enemy.p.x, enemy.p.y));
        if(path.length > maxDuration / FRAME_DURAITON) path.pop();
        lastFrame = Math.floor(enemy[KEY_OBJECT_FRAME]);
      }
    });
    return durations.map(duration => {
      return enemy => {
        if(!enemy[KEY_ENEMY_DEAD_FRAME]) {
          const pos = path[Math.floor(duration / FRAME_DURAITON) - 1];
          const prevPos = path[Math.floor(duration / FRAME_DURAITON) - 2];
          if(pos && prevPos) {
            enemy.p.x = approach(enemy.p.x, pos.x, (pos.x - prevPos.x) * $timeRatio.$);
            enemy.p.y = approach(enemy.p.y, pos.y, (pos.y - prevPos.y) * $timeRatio.$);
          }
        }
      }
    })
  };

  const graphic = (x, y, draw, animations = []) => ({
    ...object(x, y, 0, 0),
    [KEY_OBJECT_ON_UPDATE]: [
      graphic => graphic[KEY_OBJECT_FRAME] > 0 && draw(graphic),
      ...animations
    ],
  });

  const effect = (x, y, duration, draw) => graphic(x, y, (graphic) => {
    const progress = getActionProgress(graphic[KEY_OBJECT_FRAME], duration, false);
    if(progress >= 1) graphic[KEY_GRAPHIC_IS_ANIMATION_FINISH] = true;
    if(!graphic[KEY_GRAPHIC_IS_ANIMATION_FINISH]) draw(progress, graphic);
  });

  const wipe = () => effect(0, 0, 2400, (progress) => {
    draw(61, ctx => {
      const pos = cameraFrameSize.x - cameraFrameSize.x * Math.min(1, easeInQuad(progress / 0.3));
      const size = progress < 0.7 ? (cameraFrameSize.x + 10) : cameraFrameSize.x * Math.max(0, 1 - easeOutQuad((progress - 0.7) / 0.3));
      ctx.fillStyle = '#000';
      ctx.fillRect(pos, 0, size, cameraFrameSize.y);
    });
  });

  const ripple = (x, y, maxR) => effect(x, y, 3000, (progress, graphic) => {
    graphic.p.x -= $backgroundV.$ * $timeRatio.$;
    const r = transform(maxR) * progress;
    const color = (a = 0) => `rgba(70,90,110,${a})`;
    const drawRipple = (ctx, colors, ...args) => {
      const grad = ctx.createRadialGradient(
        ...transform(graphic.p),
        r * progress,
        ...transform(graphic.p),
        r
      );
      colors.forEach(color => grad.addColorStop(...color));
      ctx.fillStyle = grad;
      ctx.lineWidth = transform(10) * easeInQuint(1 - progress);
      ctx.save();
      ctx.translate(0, transform(graphic.p)[1] * 0.7);
      ctx.scale(1, 0.3);
      ctx.beginPath();
      ctx.ellipse(...transform(graphic.p), r, r, 0, ...args);
      ctx.fill();
      ctx.restore();  
    };
    
    draw(10, ctx => drawRipple(ctx, [
      [0.1, color()],
      [0.6, color(easeInQuint(1 - progress))],
      [0.61, color()],
    ], Math.PI, 2 * Math.PI));
    draw(52, ctx => drawRipple(ctx, [
      [0.6, color()],
      [0.61, color(easeInQuint(1 - progress))],
      [1, color()],
    ], 2 * Math.PI, Math.PI));
  });

  const checkRipple = isUnderWater => object => {
    if(object[KEY_OBJECT_FRAME] > 0 && $reflectionY.$ !== undefined) {
      const isNowUnderWater = object.p.y - object.s.y / 2 <= 0;
      if(isUnderWater !== isNowUnderWater) {
        if(isUnderWater !== undefined && effects.length < 10) effects.push(ripple(object.p.x, $reflectionY.$, vectorMagnitude(object.v) * 5 + 100));
        isUnderWater = isNowUnderWater;
      }
    }
  };

  const background = (draw, v) => 
    Array(3).fill().map((_, i) => 
      graphic(i * DEFAULT_FRAME_WIDTH - DEFAULT_FRAME_WIDTH, 0, (graphic) => {
        if(graphic.p.x < DEFAULT_FRAME_WIDTH * -1.5 ) graphic.p.x = DEFAULT_FRAME_WIDTH * 1.5;
        else graphic.p.x-= v * $timeRatio.$ * $backgroundV.$; 
        draw(graphic.p.x - DEFAULT_FRAME_WIDTH / 2, i);
      }));
      
  const bamboo = (x, y, h, amount, distance, zIndex, amplitude) => {
    const progress = Array(amount).fill().map(() => Math.random());
    const seeds = progress.map(() => Math.random());
    const lineDashes = seeds.map(seed => 40 * seed + 120 * (distance / 1.5));
    const bright = 180 + 60 * (distance > 1 ? -0.3 : easeOutQuad(1 - distance));
    const strokeStyle = `rgb(${bright * 0.82}, ${bright}, ${bright * 0.96})`;
    return offset => draw(zIndex, ctx => {
      ctx.strokeStyle = strokeStyle;
      for(let i = 0; i < amount; i++) {
        const seed = seeds[i];
        progress[i] = progress[i] >= 1 ? 0 : progress[i] + (0.0005 + 0.001 * seed) * $timeRatio.$;
        ctx.lineWidth = transform(2 + 10 * distance * (0.4 + 0.6 * seed), distance);
        ctx.setLineDash([transform(lineDashes[i]), transform(1.5)]);
        ctx.beginPath();
        const rootX = x + offset + DEFAULT_FRAME_WIDTH / amount * i + 70 * seed;
        const rootY = y - 20 * seed;
        ctx.moveTo(...transform(vector(rootX, rootY), distance));
        ctx.quadraticCurveTo(
          ...transform(circular(
            rootX,
            rootY + h / 2,
            0,
            h / 8,
            progress[i]
          ), distance),
          ...transform(circular(
            rootX,
            rootY + h * 0.8 + h * seed * 0.2,
            amplitude / 2 + amplitude / 2 * seed,
            0,
            progress[i]
          ), distance)
        );
        ctx.globalAlpha = distance > 1 ? 0.6 : 0.8;
        ctx.stroke();
        ctx.globalAlpha = 1;
      }
      ctx.setLineDash([]);
    });
  };

  const staticBamboo = (x, y, h, amount, distance, zIndex) => {
    const drawBamboo = bamboo(x - DEFAULT_FRAME_WIDTH / 2, y, h, amount, distance, zIndex, 50);
    return graphic(0, 0, () => drawBamboo(0));
  };

  const movingBamboo = (x, y, h, amount, distance, zIndex = 10) => {
    const drawBamboo = Array(3).fill().map(() => bamboo(x, y, h, amount, distance, zIndex, 100));
    return background((offset, index) => {
      if(distance < 1 || index !== 1) drawBamboo[index](offset);
    }, (distance > 1 ? 3 : 2) * distance);
  };

  const gradient = (y, h, z, distance, colors, depth) => graphic(0, 0, () => draw(z, ctx => {
    const grad = createLinearGradient(y, h, colors, distance, depth);
    ctx.fillStyle = grad;
    ctx.fillRect(
      0, transform(vector(0, y), distance)[1],
      cameraFrameSize.x * 2,
      transform(h, depth ? distance : undefined)
    );
  }));

  const mountainImg = decompressPath(`	qaK^LWZMGGOWGOGGO`);
  mountainImg.p[0].y = mountainImg.p[mountainImg.p.length - 1].y;
  const getMountainColor = (bright, distance, a = 1) => `rgba(${bright * (0.64 + 0.3 * (1 - distance / 0.3))}, ${bright * (0.8 + 0.1 * (1 - distance / 0.3))}, ${bright}, ${a})`;
  const drawMountain = (x, y, z, scale = 1, distance, fillGradient = true) => {
    let bright = 157 + 70 * (1 - distance / 0.4);
    draw(z, ctx => {
      ctx.fillStyle = fillGradient ? createLinearGradient(y + 400 * (1 - distance / 0.4),  -mountainImg.h, [
        [0, getMountainColor(bright * 0.9, distance)],
        [0.1, getMountainColor(bright, distance)]
      ], distance) : getMountainColor(bright, distance);
      ctx.beginPath();
      mountainImg.p.forEach(p => {
        ctx.lineTo(...transform(vector((x + p.x) * scale, (y + p.y + mountainImg.h / 2) * scale), distance));
      });
      ctx.fill();
      
      if($reflectionY.$ != undefined) {
        ctx.fillStyle = createLinearGradient(y + 500 * (1 - distance / 0.35),  mountainImg.h, [
          [0, getMountainColor(bright * 0.8, distance)],
          [0.1, getMountainColor(bright * 0.9, distance)],
          [1, getMountainColor(bright * 0.9, distance, 0.3)],
        ], distance);
        ctx.beginPath();
        mountainImg.p.forEach(p => {
          ctx.lineTo(...transform(vector((x + p.x) * scale, (y - p.y - mountainImg.h / 2 + 56) * scale), distance));
        });
        ctx.fill();
      }
    });  
  };

  const staticMountain = (x, y, z, distance, scale) => 
    graphic(x, y,
      () => drawMountain(x, y, z, scale, distance, false)
    );

  const movingMountain = (x, y, z, distance = 1, scale = 1) => background((offset, index) => {
    drawMountain(x + offset + 100 * index, y, z, scale, distance);
  }, $backgroundV.$);

  const letterBox = () => {
    const height = transform(83);
    const drawLetterbox = graphic => draw(61, ctx => {
      ctx.fillStyle = '#000';
      ctx.fillRect(0, graphic.p.y, cameraFrameSize.x + 5, height);
    });
    return [
      graphic(0, 0, drawLetterbox, [slideIn(3000, 0, -height)]),
      graphic(0, cameraFrameSize.y - height, drawLetterbox, [slideIn(3000, 0, cameraFrameSize.y)]),
    ]
  };

  const drawCaption = text => draw(62, ctx => {
    ctx.setLineDash([]);
    ctx.lineWidth = transform(0.7);
    ctx.fillStyle = '#eec918';
    ctx.strokeStyle = 'rgba(0,0,0,0.75)';
    ctx.font = `${transform(22)}px serif`;
    const args = [text, cameraFrameSize.x / 2, cameraFrameSize.y - transform(120)];
    ctx.fillText(...args);
    ctx.strokeText(...args);
  });

  const summonTheft = (x, y, z) => () => {
    const skeleton = createSkeletion();
    $theft.$ = [graphic(x, y, graphic => draw(z, ctx => {
      skeleton.p($theft.$[2]);
      skeleton.d(ctx, graphic.p, ['#8a302c', '#DB6157', '#e8e8e8', '#a4413d', '#c57777'], $theft.$[1], $theft.$[3]);
    }), [checkRipple()]), 1, POSE_CHARGE, 1];
    graphics.push($theft.$[0]);
  };

  const moveTheft = (x, y, facing = 1, pose = POSE_CHARGE, flip = 1) => {
    $theft.$[0].p.x = x;
    $theft.$[0].p.y = y;  $theft.$[1] = facing;
    $theft.$[2] = pose;
    $theft.$[3] = flip;
  };

  function drawPath(ctx, img, color, offset, angle, facing, flip, func = 'fill') {
    ctx[`${func}Style`] = color;
    ctx.beginPath();
    img.p.forEach(p => {
      ctx.lineTo(...transform(rotate(offset, vector(p.x * facing + offset.x, p.y * flip + offset.y), angle)));
    });
    ctx[func]();
  }

  const hatImg = decompressPath(`4#+};K1BLild`, -105, -35, 0.108);
  const faceImg = decompressPath(`F[is'4N`, -43, -30, 0.141);
  const bodyImg = decompressPath(`ge_G?>GOE4/'~`, -35, -170, 0.109);
  const rightThigh = decompressPath(`reks&<?;GGK`, -20, 70, 0.108);
  const rightCalf = decompressPath(`p'>GDJGG;OJge~`, -50, -115, 0.1);
  const leftThigh = decompressPath(`,te{%=DODOEJ`, -63, 55, 0.1);
  const leftCalf = decompressPath(`+;Lcgk}r+5G;MGO`, -57, 60, 0.1);
  const leftHand = decompressPath(` )nN/`, -15, -20, 0.102);
  const leftUpperArm = decompressPath(`WLjKl&G>=`, -27, 13, 0.108);
  const rightHand = decompressPath(`,.+Eek"`, -70, -45, 0.1145);
  const rightUpperArm = decompressPath('+??=OO`Od#', -37, -90, 0.103);
  const sword = decompressPath(`aAeA%B!%''''cggggc`, -113, 10, 0.216);

  function createSkeletion() {
    const body = [vector(0, 11.9), 0];
    const head = [vector(0, 12.7), 0];
    const rightLeg = [vector(-2.3, -6.7), 0];
    const leftLeg = [vector(1.9, -7), 0];
    const rightArm = [vector(-5.2, 5.1), 0];
    const leftArm = [vector(5.6, 5.8), 0];
    const leftKnee = [vector(2.9, -12), 0];
    const rightKnee = [vector(-0.8, -11.9), 0];
    const swordJoint = [vector(3.9, -14), -1.124];
    
    const j = [body, head, rightLeg, leftLeg, rightArm, leftArm, leftKnee, rightKnee, swordJoint];
    
    const p = [
      [leftUpperArm, 0, [j[0], j[5]]],
      [sword, 1, [j[0], j[5], j[8]]],
      [leftHand, 2, [j[0], j[5], [vector(3,-13.7), 0, j[5]]]],
      [bodyImg, 3, [j[0]]],
      [faceImg, 2, [j[0], j[1]]],
      [hatImg, 3, [j[0], j[1], [vector(0.2, 3), 0, j[1]]]],
      [leftCalf, 3, [j[0], j[3], j[6]]],
      [leftThigh, 3, [j[0], j[3]]],
      [rightCalf, 3, [j[0], j[2], j[7]]],
      [rightThigh, 3, [j[0], j[2]]],
      [rightUpperArm, 4, [j[0], j[4]]],
      [rightHand, 2, [j[0], j[4], [vector(0.4, -16.3), 0, j[4]]]]
    ];
    
    return {
      j,
      d: (ctx, center, colors, facing = 1, flip = 1) => p.forEach(([img, colorIndex, joints]) => {
        const rotationDirection = (facing / Math.abs(facing)) * (flip / Math.abs(flip));
        let pos = vector(center.x, center.y);
        joints.forEach(([offset], index) => {
          const prevPos = vector(pos.x, pos.y);
          pos.x += offset.x * facing;
          pos.y += offset.y * flip;
          if(index > 0) pos = rotate(prevPos, pos, joints[index - 1][1] * rotationDirection);
        });
        const angle = joints[joints.length - 1][2] ? joints[joints.length - 1][2][1] : joints[joints.length - 1][1];
        drawPath(ctx, img, colors[colorIndex], pos, angle * rotationDirection, facing, flip); 
      }),
      p: angles => angles.forEach((angle, index) => j[index][1] = angle)
    };
  }

  function drawDragTrack(fromX, fromY, toX, toY, opacity = 0.2) {
    draw(61, ctx => {
      // visualize drag track
      const grad = ctx.createLinearGradient(fromX, fromY, toX, toY);
      grad.addColorStop(0, `rgba(255,255,255,${opacity})`);
      grad.addColorStop(0.75, `rgba(255,255,255,0)`);
      ctx.strokeStyle = grad;
      ctx.lineCap = 'round';
      ctx.lineWidth = 30;
      ctx.beginPath();
      ctx.moveTo(fromX, fromY);
      ctx.lineTo(toX, toY);
      ctx.stroke();
      ctx.lineCap = 'butt';
    });
  }

  const LETTER_S = `4DUME<#`;
  const LETTER_O = `r":<FDOJa`;
  const scale = 0.6;
  const letters = [
    decompressPath(LETTER_S, -10, 0, scale),
    decompressPath(`	 'fe%`, -70, 85, scale),
    decompressPath(LETTER_O, -150, 88, scale),
    decompressPath(`#`, -230, 97, scale),
    decompressPath(`%c#c`, -275, 90, scale),
    decompressPath(`GOOM`, -290, 90, scale),
    decompressPath(LETTER_S, -340, 0, scale),
    decompressPath(`GGGE|GFNL`, -380, 5, scale),
    decompressPath(LETTER_O, -500, 88, scale),
    decompressPath(`[BD3OML`, -545, 95, scale),
    decompressPath(`[UFF?,`, -580, 90, scale)
  ];

  function drawTitle(opacity) {
    draw(61, ctx => {
      const color = `#fff`;
      ctx.lineJoin = 'bevel';
      ctx.lineWidth = easeInQuad(opacity) * transform(3);
      ctx.setLineDash([50 * easeInOutQuint(opacity), transform(100) * easeInOutQuad(1 - opacity)]);
      letters.forEach(img => drawPath(ctx, img, color, vector(-160, $titleY.$), 0, 1, 1, 'stroke'));
      ctx.lineJoin = 'miter';
    });
  }

  // window.addEventListener('keydown', ({ key }) => pressingKeys.add(key));
  // window.addEventListener('keyup', ({ key }) => pressingKeys.delete(key));

  function onPressMove({ clientX, clientY }) {
    cursorPos.x = clientX - $canvasLeftOffset.$;
    cursorPos.y = clientY;
  }

  function onPressDown({ clientX, clientY }) {
    cursorPos.x = clientX - $canvasLeftOffset.$;
    cursorPos.y = clientY;
    pressDownPos.x = clientX - $canvasLeftOffset.$;
    pressDownPos.y = clientY;
    $isPressing.$ = true;
    resolveClick();
    if(isAbleToDash()) slowDown();
    resumeAudio();
    player[KEY_PLAYER_CHARGE_FRAME] = player[KEY_OBJECT_FRAME];
  }

  function onPressUp() {
    $isPressing.$ = false;
    dash();
    backToNormal();
  }

  window.addEventListener('mousemove', onPressMove);
  window.addEventListener('mousedown', onPressDown);
  window.addEventListener('mouseup', onPressUp);
  window.addEventListener('touchstart', ({ touches }) => onPressDown(touches[0]));
  window.addEventListener('touchmove', ({ touches }) => onPressMove(touches[0]));
  window.addEventListener('touchend', ({ touches }) => onPressUp(touches[0]));

  var interaction = () => {
    // draw(61, ctx => {
    //   let x = cursorPos.x;
    //   let y = cursorPos.y - 10;
    //   if (x > cameraFrameSize.x - 40) x -= 40;
    //   if (y < 40) y += 60;
    
    //   ctx.font = `10px sans-serif`;
    //   ctx.fillStyle = '#fff';
    //   ctx.fillText(
    //     `${(cursorPos.x).toFixed()}, ${cursorPos.y.toFixed()}`,
    //     x,
    //     y
    //   );
    //   const worldPos = detransform(vector(cursorPos.x, cursorPos.y));
    //   ctx.fillText(`${worldPos.x.toFixed()}, ${worldPos.y.toFixed()}`, x, y - 15);
    // });

    if ($isPressing.$) drawDragTrack(pressDownPos.x, pressDownPos.y, cursorPos.x, cursorPos.y);
  };

  // window.addEventListener('keydown', ({ key }) => {
  //   if(key === 'Escape') $timeRatio.$ = $timeRatio.$ === 0 ? NORAML_TIME_RATIO : 0;
  // });

  var time = (ctx) => {
    // execute animation
    for(let i = animations.length - 1; i >= 0; i--) {
      const [id, callback, shouldStop] = animations[i];
      callback();
      if(shouldStop()) removeAnimation(id);
    }
  };

  var background$1 = ctx => {
    ctx.fillStyle = $backgroundColor.$ || '#000';
    ctx.fillRect(0, 0, cameraFrameSize.x + 1, cameraFrameSize.y);
    drawStack.forEach(layer => {
      while(layer.length > 0) {
        layer.shift()(ctx);
      }
    });
  };

  function objectLoop(object, ctx) {
    let collidedSide;
    if (object[KEY_OBJECT_ON_COLLIDED]) {
      const objBoundary = getObjectBoundary(object);
      collidedSide = collision(player, object);
      
      object[KEY_OBJECT_IS_COLLIDED] = !!collidedSide;
      object[KEY_OBJECT_ON_COLLIDED](object, objBoundary, collidedSide);
    }
    if (object[KEY_OBJECT_ON_UPDATE])
      object[KEY_OBJECT_ON_UPDATE].forEach((onUpdate) => onUpdate(object, ctx));
    object[KEY_OBJECT_FRAME] += 1 * $timeRatio.$;
    return collidedSide;
  }

  var objects = (ctx) => {
    for (let i = enemies.length - 1; i >= 0; i--) {
      // remove enemy when dead
      if (enemies[i][KEY_ENEMY_IS_DEAD]) enemies.splice(i, 1);
      else objectLoop(enemies[i], ctx);
    }
    $playerCollisionSide.$ = {};
    for (let i = platforms.length - 1; i >= 0; i--) {
      const collsionSide = objectLoop(platforms[i], ctx);
      if(collsionSide) $playerCollisionSide.$[collsionSide] = 1;
    }
    for (let i = projectiles.length - 1; i >= 0; i--) {
      if (
        isOutOfScreen(projectiles[i]) ||
        projectiles[i][KEY_PROJECTILE_SORUCE] && projectiles[i][KEY_PROJECTILE_SORUCE][KEY_ENEMY_IS_DEAD] ||
        projectiles[i][KEY_PROJECTILE_IS_COMSUMED] ||
        projectiles[i].p.y <= - projectiles[i].s.y
      )
        projectiles.splice(i, 1);
      else objectLoop(projectiles[i], ctx);
    }
    if ($stage.$) objectLoop($stage.$, ctx);
    objectLoop(player, ctx);
    for (let i = graphics.length - 1; i >= 0; i--) {
      if (graphics[i][KEY_GRAPHIC_IS_ANIMATION_FINISH]) graphics.splice(i, 1);
      else objectLoop(graphics[i], ctx);
    }
    for (let i = effects.length - 1; i >= 0; i--) {
      if (effects[i][KEY_GRAPHIC_IS_ANIMATION_FINISH]) effects.splice(i, 1);
      else objectLoop(effects[i], ctx);
    }
  };

  const projectile = (enemy, v) => ({
    ...object(enemy.p.x, enemy.p.y, 10, 10, v.x, v.y),
    [KEY_PROJECTILE_SORUCE]: enemy,
    [KEY_OBJECT_ON_COLLIDED]: (projectile, projectileBoundary, collidedSide) => {
      if (collidedSide) {
        playerDamage();
        projectile[KEY_PROJECTILE_IS_COMSUMED] = true;
      }
    },
    [KEY_OBJECT_ON_UPDATE]: [
      projectile => {
        vectorOp(
          (pos, v) => pos + v * $timeRatio.$,
          [projectile.p, projectile.v],
          projectile.p
        );
        draw(35, (ctx) => {
          ctx.fillStyle = '#ec5751';
          const { l, t } = getObjectBoundary(projectile);
          ctx.fillRect(
            ...transform(vector(l, t)),
            transform(projectile.s.x),
            transform(projectile.s.y)
          );
      
          if (isUnderWater(projectile)) {
            ctx.fillStyle = $reflectionGradient.$;
            ctx.fillRect(
              ...transform(vector(l, t)),
              transform(projectile.s.x),
              transform(projectile.s.y)
            );
          }
      
          const reflection = getReflection(projectile);
          if (reflection) {
            ctx.fillStyle = '#ec5751';
            ctx.globalAlpha = 0.2;
            ctx.fillRect(
              reflection.x - transform(projectile.s.x) / 2,
              reflection.y,
              transform(projectile.s.x),
              transform(projectile.s.y)
            );
            ctx.globalAlpha = 1;
          }
        });
      },
      checkRipple(),
    ],
  });

  // rendering
  const getEnemyColor = (enemy) => {
    if (
      enemy[KEY_OBJECT_IS_COLLIDED] &&
      Math.round(enemy[KEY_OBJECT_FRAME]) % 4 > 1
    ) {
      return `rgba(200,200,200,${0.9 * getDeathProgress(enemy)})`;
    } else if (enemy[KEY_ENEMY_IS_UNTOUCHABLE]) {
      return `#ec5751`;
    } else {
      return `#464646`;
    }
  };

  const getDeathProgress = (enemy) => {
    let deathProgress =
      1 -
      getActionProgress(
        enemy[KEY_OBJECT_FRAME] - enemy[KEY_ENEMY_DEAD_FRAME],
        ENEMY_DEATH_ANIMATION_DURATION,
        false
      );
    deathProgress = isNaN(deathProgress) ? 1 : deathProgress;
    return Math.max(0, deathProgress);
  };

  const drawEnemyShell = (ctx, enemy, width) => {
    const angle = vectorAngle(enemy.p, player.p) / Math.PI / 2;
    if (enemy[KEY_ENEMY_IS_DEFENCING]) {
      ctx.setLineDash([
        transform(30), 
        transform(60 * (1 - (enemy[KEY_ENEMY_HEALTH] - 1) / 2))
      ]);
      ctx.lineWidth = transform(width);
      ctx.lineCap = 'round';
      ctx.beginPath();
      ctx.ellipse(
        ...transform(enemy.p),
        transform(24),
        transform(24),
        -Math.PI * 0.45 - 2 * Math.PI * angle,
        0,
        Math.PI * 0.9
      );
      ctx.stroke();
    }
  };

  function drawEnemy(enemy) {
    if (enemy[KEY_OBJECT_FRAME] === 0) return;
    draw(enemy[KEY_OBJECT_Z_INDEX], (ctx) => {
      const color = getEnemyColor(enemy);
      ctx.globalAlpha =
        easeInQuad(getDeathProgress(enemy)) *
        (!enemy[KEY_ENEMY_IS_UNTOUCHABLE] && enemy[KEY_ENEMY_COMPUND_PARENT]
          ? 0.3
          : 1);
      ctx.fillStyle = color;
      ctx.strokeStyle = 'rgba(0,0,0,0.2)';
      ctx.font = `bold ${transform(36)}px sans-serif`;
      ctx.fillText(enemy[KEY_ENEMY_APPEARANCE], ...transform(enemy.p));
      drawEnemyShell(ctx, enemy, 4);
      ctx.globalAlpha = 1;
      
      const angle = vectorAngle(enemy.p, player.p) / Math.PI / 2;
      const eyeCenter = vector(enemy.p.x + 0, enemy.p.y + 10);
      const eyePos = circular(eyeCenter.x, eyeCenter.y, 1.6, 1.6, angle);
      ctx.beginPath();
      ctx.fillStyle = '#eee';
      ctx.ellipse(
        ...transform(eyeCenter),
        transform(4),
        transform(4),
        0,
        0,
        2 * Math.PI
      );
      ctx.fill();
      ctx.beginPath();
      ctx.fillStyle = '#ec5751';
      ctx.ellipse(
        ...transform(eyePos),
        transform(2.4),
        transform(2.4),
        0,
        0,
        2 * Math.PI
      );
      ctx.fill();

      if (isUnderWater(enemy)) {
        ctx.globalAlpha = 1;
        ctx.fillStyle = $reflectionGradient.$;
        ctx.strokeStyle = $reflectionGradient.$;
        ctx.fillRect(...transform(vector(enemy.p.x - 18, enemy.p.y + 18)), transform(36), transform(36));
        drawEnemyShell(ctx, enemy, 6);
      }
    });
    draw(enemy[KEY_OBJECT_Z_INDEX] + 1, (ctx) => {
      const reflection = getReflection(enemy);
      if (reflection) {
        ctx.fillStyle = getEnemyColor(enemy);
        ctx.globalAlpha = 0.3 * reflection.d;
        let appearance = enemy[KEY_ENEMY_APPEARANCE];
        if(appearance == '士') appearance = '干';
        else if(appearance == '干') appearance = '士';
        else if(appearance == '由') appearance = '甲';
        ctx.fillText(appearance, reflection.x, reflection.y);
        ctx.globalAlpha = 1;
      }
    });
  }

  // class
  const _enemy = (x, y, w, h, options = {}) => ({
    ...object(x, y, w, h),
    [KEY_OBJECT_ON_COLLIDED](enemy, enemyBoundary, collidedSide) {
      if (!collidedSide || enemy[KEY_ENEMY_DEAD_FRAME]) return;
      
      if (enemy[KEY_ENEMY_IS_UNTOUCHABLE]) {
        playerDamage();
      } else {
        player[KEY_PLAYER_ATTACK_FRAME] = player[KEY_OBJECT_FRAME];
        if (enemy[KEY_ENEMY_IS_DEFENCING]) {
          // bounce back
          if (collidedSide === SIDE_T || collidedSide === SIDE_B) {
            setDash(1);
            player.v.y *= -0.5;
            player.p.y =
              enemyBoundary[collidedSide] +
              (player.s.y / 2) * (collidedSide === SIDE_T ? 1 : -1);
          } else if (collidedSide === SIDE_L || collidedSide === SIDE_R) {
            setDash(1);
            player.v.x *= -0.5;
            player.p.x =
              enemyBoundary[collidedSide] +
              (player.s.x / 2) * (collidedSide === SIDE_R ? 1 : -1);
          }
          playSound(2);
        } else {
          if(enemy[KEY_ENEMY_COMPUND_PARENT]) {
            playSound(0, 500);
          } else {
            playSound(0, FRAME_DURAITON);
            const pos = vector(enemy.p.x, enemy.p.y);
            const vm = vectorMagnitude(player.v);
            const v = vector(player.v.x / vm, player.v.y / vm);
            effects.push(effect(0, 0, 300, (progress) => {
              progress = easeOutCirc(progress);
              draw(61, ctx => {
                ctx.lineWidth = 10 * (1 - progress);
                ctx.strokeStyle = `rgba(0,0,0,${1 - progress})`;
                ctx.beginPath();
                ctx.moveTo(...transform(vectorOp((a, b) => a - b * 200 * progress, [pos, v])));
                ctx.lineTo(...transform(vectorOp((a, b) => a + b * 200 * progress, [pos, v])));
                ctx.stroke();
              });
            }));
          }
        }
        // take damage
        setDash(1);
        if (enemy[KEY_OBJECT_FRAME] - enemy[KEY_ENEMY_LAST_DAMAGE_FRAME] > 20) {
          enemy[KEY_ENEMY_HEALTH]--;
          enemy[KEY_ENEMY_LAST_DAMAGE_FRAME] = enemy[KEY_OBJECT_FRAME];
          if (enemy[KEY_ENEMY_HEALTH] === 0) {
            enemy[KEY_ENEMY_DEAD_FRAME] = enemy[KEY_OBJECT_FRAME];
            vectorOp((v) => v * 0.3, [player.v], enemy.v);
          }
        }
      }
    },
    [KEY_ENEMY_LAST_DAMAGE_FRAME]: -1,
    [KEY_OBJECT_Z_INDEX]: 15,
    ...options,
    [KEY_ENEMY_HEALTH]: options[KEY_ENEMY_HEALTH] || 1,
    [KEY_OBJECT_ON_UPDATE]: [
      ...(options[KEY_OBJECT_ON_UPDATE] || []),
      drawEnemy,
      (enemy) => {
        if (enemy[KEY_ENEMY_DEAD_FRAME]) {
          enemy.v.y -= ($g.$ / 2) * $timeRatio.$;
          enemy.v.x -= $backgroundV.$ * 0.1 * $timeRatio.$;
          enemy.p.x += enemy.v.x * $timeRatio.$;
          enemy.p.y += enemy.v.y * $timeRatio.$;
        }
      },
      objectEvent(
        (enemy) => {
          enemy[KEY_ENEMY_IS_DEAD] = true;
        },
        ENEMY_DEATH_ANIMATION_DURATION,
        {
          [KEY_OBJECT_EVENT_IS_REPEAT]: false,
          [KEY_OBJECT_EVENT_GET_OFFSET]: (enemy) => enemy[KEY_ENEMY_DEAD_FRAME],
        }
      ),
      checkRipple(),
    ],
  });

  const compund = (core, ...children) => {
    core[KEY_OBJECT_ON_UPDATE].push((enemy) => {
      children.forEach((child) => {
        child[KEY_ENEMY_COMPUND_PARENT] = core;
        child[KEY_ENEMY_HEALTH] = Math.max(2, child[KEY_ENEMY_HEALTH]);
        if (enemy[KEY_ENEMY_DEAD_FRAME] && !child[KEY_ENEMY_DEAD_FRAME])
          child[KEY_ENEMY_DEAD_FRAME] = child[KEY_OBJECT_FRAME];
      });
    });
    return [core, ...children];
  };

  const chain = (head, amount, interval, coreIndex, getEnemy) => {
    let nodes = [
      head,
      ...chase(
        head,
        Array(amount)
          .fill()
          .map((_, i) => (i + 1) * interval)
      ).map((doChase, i) => {
        const enemy = getEnemy(i, head);
        enemy[KEY_OBJECT_ON_UPDATE].push(doChase);
        return enemy;
      }),
    ];
    nodes = compund(...nodes.splice(coreIndex, 1), ...nodes);
    nodes.splice(coreIndex, 0, nodes.splice(0, 1)[0]);
    return nodes;
  };

  const enemy = (appearance, x, y, actions, isUntouchable) =>
    _enemy(x, y, 30, 30, {
      [KEY_OBJECT_ON_UPDATE]: actions,
      [KEY_ENEMY_APPEARANCE]: appearance,
      [KEY_ENEMY_IS_UNTOUCHABLE]: isUntouchable
    });

  const shell = (appearance, x, y, actions) =>
    _enemy(x, y, 40, 40, {
      [KEY_OBJECT_ON_UPDATE]: [
        enemy => (enemy[KEY_ENEMY_IS_DEFENCING] = enemy[KEY_ENEMY_HEALTH] > 1),
        ...actions
      ],
      [KEY_ENEMY_APPEARANCE]: appearance,
      [KEY_ENEMY_HEALTH]: 3
    });

  const firework = (amount, interval, startTime = 0, angle = 0) =>
    objectEvent(
      (enemy) => {
        if (!enemy[KEY_ENEMY_DEAD_FRAME]) {
          const v = 2;
          for (let i = 0; i < amount; i++) {
            const theta = (i / amount - angle) * 2 * Math.PI;
            projectiles.push(projectile(enemy, vector(v * Math.cos(theta), v * Math.sin(theta))));
          }
        }
      },
      interval,
      {
        [KEY_OBJECT_EVENT_FIRST_FRAME_TRIGGER]: true,
        [KEY_OBJECT_EVENT_GET_OFFSET]: () => startTime / FRAME_DURAITON,
      }
    );

  function followPlayerY(platform) {
    platform.p.y = player.p.y;
  }

  function handleStandardColiision(platform, platformBoundary, collidedSide) {
    const playerBoundary = getObjectBoundary(player);
    if (collidedSide === SIDE_T) resetDash();
    if (
      (collidedSide === SIDE_B || collidedSide === SIDE_T) &&
      playerBoundary.l < platformBoundary.r - 1 &&
      playerBoundary.r > platformBoundary.l + 1
    ) {
      player.v.y = platform.v.y;
      player.p.y =
        platformBoundary[collidedSide] +
        (player.s.y / 2) * (collidedSide === SIDE_T ? 1 : -1);
      player.v.x = approach(
        player.v.x,
        platform.v.x,
        (platform.v.x - player.v.x) * GROUND_FRICTION * $timeRatio.$
      );
    } else if (collidedSide === SIDE_L || collidedSide === SIDE_R) {
      resetDash();
      player.v.x = platform.v.x;
      player.p.x =
        platformBoundary[collidedSide] +
        (player.s.x / 2 - 1) * (collidedSide === SIDE_R ? 1 : -1);
      player.v.y = approach(
        player.v.y,
        platform.v.y,
        (platform.v.y - player.v.y) * WALL_FRICTION * $timeRatio.$
      );
    }
  }

  function handleBoundaryCollision(platform, platformBoundary, collidedSide) {
    if (collidedSide === SIDE_B || collidedSide === SIDE_T) {
      player.v.y = platform.v.y;
      player.p.y =
        platformBoundary[collidedSide] +
        (player.s.y / 2) * (collidedSide === SIDE_T ? 1 : -1);
    } else if (collidedSide === SIDE_L || collidedSide === SIDE_R) {
      player.v.x = platform.v.x;
      player.p.x =
        platformBoundary[collidedSide] +
        (player.s.x / 2) * (collidedSide === SIDE_R ? 1 : -1);
    }
  }

  // function drawPlatform(platform) {
  //   if (platform[KEY_OBJECT_FRAME] === 0 || !pressingKeys.has('1')) return;
  //   draw(31, (ctx) => {
  //     const platformBoundary = getObjectBoundary(platform);
  //     ctx.lineWidth = 1;
  //     ctx.strokeStyle = '#fff';
  //     ctx.strokeRect(
  //       ...transform(vector(platformBoundary.l, platformBoundary.t)),
  //       transform(platform.s.x),
  //       transform(platform.s.y)
  //     );
  //   });
  // }

  const platform = (x, y, w, h, actions, onCollided = handleStandardColiision, forceCheckCollision) => ({
    ...object(x, y, w, h),
    [KEY_OBJECT_ON_COLLIDED]: onCollided,
    [KEY_OBJECT_ON_UPDATE]: actions,
    [KEY_OBJECT_FORCE_CHECK_COLLISION]: forceCheckCollision
    // [KEY_OBJECT_ON_UPDATE]: [
    //   drawPlatform,
    //   ...actions,
    // ],
  });  

  let bambooCycle = 0;
  const getBambooCycleDuration = () => 8000 + 1000 * (bambooCycle++ % 3);

  const horizontalBamboo = (x, y, w) =>
    platform(x, y, w, 0, [
      circularMovement(getBambooCycleDuration(), 0, 15),
      (platform) => draw(20, (ctx) => {
        const { l, t, r } = getObjectBoundary(platform);
        const direction = x < 0 ? -1 : 1;
        const grad = ctx.createLinearGradient(
          ...transform(vector(l - 20, 0)),
          ...transform(vector(r + 20, 0))
        );
        grad.addColorStop(0.1, '#B6D8D2');
        grad.addColorStop(0.5, '#4E8F80');
        grad.addColorStop(0.9, '#B6D8D2');
        ctx.strokeStyle = grad;
        ctx.lineWidth = transform(5);
        ctx.setLineDash([transform(80), transform(1)]);

        const side = x < 0 ? l : r;
        const anotherSide = x < 0 ? r : l;
        ctx.beginPath();
        ctx.moveTo(...transform(vector(x + direction * w * 4, y - w * 2)));
        ctx.quadraticCurveTo(
          ...transform(vector(side + direction * w * 2, t)),
          ...transform(vector(side, t))
        );
        ctx.quadraticCurveTo(
          ...transform(vector(anotherSide - (direction * w) / 4, t + 2)),
          ...transform(vector(anotherSide - (direction * w) / 2, t - w * 0.1))
        );
        ctx.stroke();
      }),
    ], (platform, platformBoundary, collidedSide) => {
      if (collidedSide) setDash(Math.max($dash.$, 1));
      if (collidedSide === SIDE_T && player.v.y <= 0)
        handleStandardColiision(platform, platformBoundary, collidedSide);
    });

  const verticalBamboo = (x, y, h) =>
    platform(x, y, 7, h, [
      circularMovement(getBambooCycleDuration(), 15, 0),
      (platform) => {
        draw(20, (ctx) => {
          const { t, b } = getObjectBoundary(platform);
          const startY = b - h;
          const endY = t + h / 2;
          const grad = createLinearGradient(startY, startY - endY, [
            [0, 'rgba(221,234,240, 0)'],
            [0.2, '#B6D8D2'],
            [0.4, '#B6D8D2'],
            [0.6, '#4E8F80'],
            [0.8, '#B6D8D2'],
          ]);
          ctx.strokeStyle = grad;
          ctx.lineWidth = transform(7);
          ctx.setLineDash([transform(80), transform(1)]);

          ctx.beginPath();
          ctx.moveTo(...transform(vector(x, startY)));
          ctx.lineTo(
            ...transform(vector(platform.p.x + (platform.p.x - x) / 2, endY))
          );
          ctx.stroke();
        });
      },
    ]);

  const water = (...args) => 
    platform(...args, [], (platform, platformBoundary, collidedSide) => {
      if (collidedSide) {
        if (player.v.y < 0) resetDash();
        player.p.x -= $backgroundV.$ * $timeRatio.$;
        player.v.x = approach(player.v.x, 0, player.v.x * 0.1 * $timeRatio.$);
        player.v.y = approach(
          player.v.y,
          0,
          player.v.y * (player.v.y > 0 ? 0.1 : 0.6) * $timeRatio.$
        );
      }
    });

  const flow = (x, y, w, h, v, z) => 
    platform(x, y, w, h, [
      (platform) => {
        if (platform[KEY_OBJECT_FRAME] === 0) return;
        draw(z, (ctx) => {
          const platformBoundary = getObjectBoundary(platform);
          const progress = 1 - getActionProgress(platform[KEY_OBJECT_FRAME], 300);
          ctx.fillStyle = platform.s.x > platform.s.y ? `rgba(255,255,255,0.6)` : createLinearGradient(
            platform.p.y + platform.s.y / 2 + progress * platform.s.y / 10 ,
            platform.s.y,
            Array(20).fill().map((_, i) => [i * 0.05, `rgba(255,255,255,${i % 2 == 0 ? 0.8 : 0.6})`]),
            1
          );
          ctx.fillRect(
            ...transform(vector(platformBoundary.l, platformBoundary.t)),
            transform(platform.s.x),
            transform(platform.s.y)
          );
        });
      },
    ], (platform, platformBoundary, collidedSide) => {
      if (collidedSide) {
        vectorOp(
          (player, target) => player + target * $timeRatio.$,
          [player.v, v],
          player.v
        );
      }
    }, 1);
    
  const boundarySet = (groundY = -player.s.y / 2) => [
    platform(0, groundY, DEFAULT_FRAME_WIDTH * 2, 0),
    platform(DEFAULT_FRAME_WIDTH / 2, 0, 0, player.s.y * 10, [followPlayerY], handleBoundaryCollision, 1),
    platform(-DEFAULT_FRAME_WIDTH / 2, 0, 0, player.s.y * 10, [followPlayerY], handleBoundaryCollision, 1)
  ];

  let tempStateFrame;
  const getAliveEnemies = () => enemies.filter(enemy => !enemy[KEY_ENEMY_DEAD_FRAME]);

  var stage1 = {
    [KEY_STAGE_INITIATE]() {
      $titleY.$ = 380;
      $backgroundColor.$ = '#ddeaf0';
      player.p.x = -260;
      cameraCenter.y = player.p.y + 200;
      $cameraLoop.$ = () => {
        cameraCenter.y = Math.min(
          player.p.y - player.s.y / 2 + 200,
          Math.max(player.p.y + player.s.y / 2 - 200, cameraCenter.y)
        );
      };
      $backgroundV.$ = 1;
      platforms.push(...boundarySet());
      graphics.push(
        gradient(200, 400, 0, 0.5, [
          [0, '#ddeaf0'],
          [0.9, 'rgba(104,158,131,0.6)'],
        ]),
        ...movingBamboo(0, -40, 1250, 1, 1.5, 51),
        ...movingBamboo(50, -50, 1250, 1, 1.1, 51),
        ...movingBamboo(0, 30, 1250, 5, 0.9),
        ...movingBamboo(50, 30, 1250, 5, 0.75, 8),
        ...movingBamboo(20, 30, 1250, 5, 0.6, 8)
      );
    },
    [KEY_STAGE_WAVES]: [
      () => {
        if(needTutorial) {
          tempStateFrame = $stage.$[KEY_OBJECT_FRAME];
          graphics.push(graphic(0,0, () => draw(61, () => {
            if(!$isPressing.$) {
              const progress = getActionProgress($stage.$[KEY_OBJECT_FRAME] - tempStateFrame, 1500 * $timeRatio.$);
              drawDragTrack(
                ...transform(vector(123, 16)),
                ...transform(vector(lerp(120, 99, easeInOutQuint(progress)), lerp(-5, -154, easeInOutQuint(progress)))),
                0.5 * easeOutQuad(progress)
              );
            }
            if(getAliveEnemies().length === 0) graphics.pop();
          })));
        }
        return [
          enemy('大', 50, 150, [
            slideIn(2000, 250, 200),
            circularMovement(3000, 10, 5, 2000),
          ])
        ];
      },
      () => {
        if(needTutorial) {
          let flag = false;
          let prevEnemyCount;
          graphics.push(graphic(0,0, () => draw(61, () => {
            const aliveEnemies = getAliveEnemies();
            if(aliveEnemies.length === 0) return graphics.pop();
            if(flag) {
              if(!$isPressing.$) {
                const progress = getActionProgress($stage.$[KEY_OBJECT_FRAME] - tempStateFrame, 1500 * $timeRatio.$);
                drawDragTrack(
                  ...transform(vector(120, -5)),
                  ...transform(vector(
                    lerp(120, 120 + (player.p.x - aliveEnemies[0].p.x) / 3, easeInOutQuint(progress)),
                    lerp(-5, -154 + (player.p.y - aliveEnemies[0].p.y) / 3, easeInOutQuint(progress))
                  )),
                  0.5 * easeOutQuad(progress)
                );
              }
              if(player.p.y <= 120) {
                flag = false;
                if(!$isPressing.$) backToNormal();
              }
              if(prevEnemyCount != aliveEnemies.length && aliveEnemies.length == 1) {
                slowDown(0.01);
                prevEnemyCount = aliveEnemies.length;
              }
            } else {
              if(player.p.y > 160 && Math.abs(player.v.y) < 1) {
                flag = true;
                slowDown(0.01);
                tempStateFrame = $stage.$[KEY_OBJECT_FRAME];
              }
            }
          })));
        }
        return [
          enemy('不', -100, 300, [
            slideIn(2000, 250, 350),
            circularMovement(5000, 10, 5, 2000),
          ]),
          enemy('丕', 75, 350, [
            slideIn(2000, 250, 450),
            circularMovement(3000, 10, 5, 2000),
          ]),
        ];
      },
      () =>
        compund(
          enemy('父', 0, 450, [
            slideIn(2000, 250, 330),
            circularMovement(5000, 10, 0, 2000),
          ]),
          enemy('子', 0, 300, [
            slideIn(1000, 250, 300),
            circularMovement(6000, 100, 50, 1000),
          ])
        ),
      () => [
        enemy('火', 0, 350, [
          slideIn(1500, 250, 400),
          firework(1, 2000, 1000, 0.25),
          circularMovement(10000, 100, 10, 1500),
        ]),
      ],
      () =>
        compund(
          enemy('凸', 0, 300, [
            firework(1, 2000, 1000, 0.25),
            slideIn(2000, 250, 300),
            circularMovement(6000, 150, 10, 2000),
          ]),
          enemy(
            '凹',
            0,
            220,
            [slideIn(1000, 250, 220), circularMovement(5000, 100, 10, 1000)],
            true
          )
        ),
      () => [
        enemy('林', 0, 380, [
          slideIn(3000, 250, 350),
          // circularMovement(4000, 10, 10, 3000)
        ]),
        ...Array(6).fill().map((_, i) =>  enemy('木', -50 + i * 20, 200 + i % 2 * 100, [
          firework(1, 3000, 1000 + i % 2 * 1000, 0.25),
          slideIn(2000 + i % 2 * 1000, 250, 200 + i % 2 * 100),
          circularMovement(8000, 150, 10 * Math.random() * 5, 2000 + i % 2 * 3000),
        ])),
      ],
      () => compund(
        enemy('丌', 0, 300, [
          slideIn(5000, 250, 500),
          circularMovement(10000, 150, 10, 5000)
        ]),
        ...Array(6).fill().map((_, i) =>  enemy(i % 2 === 0 ? '屮' : '千', -180 + i * 70, 150, [
          slideIn(2500 + i * 500, 250, 500 + i * i * 30),
          circularMovement(6000, 5, 150, 2500 + i * 500 + Math.random() * 2000),
        ], true)),
      ),
      () => compund(
        enemy('丼', 0, 300, [
          slideIn(6000, 120, 500),
          circularMovement(10000, 5, 5, 6000)
        ]),
        ...chain(enemy('井', 0, 300, [
          slideIn(2000, 250, 450),
          circularMovement(3000, 100, 100, 2000),
        ], 1), 12, 200, 0, i => enemy('井', 250, 450, [], 1))
      ),
    ],
    [KEY_STAGE_IS_WAVE_CLEAN]() {
      return enemies.length === 0 && Math.round(player.p.y) <= 0;
    },
    [KEY_STAGE_TRANSITION](progress) {
      const movementProgress = 1 - easeInOutQuad(alternateProgress(progress));
      $backgroundV.$ = 1 + movementProgress * 3;
      $cameraZoom.$ = 1 + movementProgress * 0.1;
      if (progress == 0) $tempPlayerPos.$ = vector(player.p.x, player.p.y);
      else player.p.x = $tempPlayerPos.$.x * easeInOutQuad(1 - progress);
    },
    [KEY_STAGE_ENDING_CUT_SCENE]: [
      [
        () => {
          $tempPlayerPos.$ = vector(player.p.x, player.p.y);
          graphics.push(...letterBox());
        },
      ],
      [
        (progress) => {
          $backgroundV.$ = 1 + easeOutQuad(progress) * 2;
          player.p.x =
            $tempPlayerPos.$.x +
            (-100 - $tempPlayerPos.$.x) * easeInOutQuad(progress);
        },
        2000,
      ],
      [() => drawCaption("Can't find the theft. I think I lost him."), 500, true],
      [summonTheft(-260, 100, 9)],
      [
        (progress) =>
          moveTheft(
            -250 + 200 * progress,
            200 * easeOutQuad(1 - alternateProgress(progress * 0.8))
          ),
        1000,
      ],
      [
        (progress) =>
          moveTheft(
            -50 + 100 * progress,
            160 + 100 * easeOutQuad(1 - alternateProgress(progress * 0.8))
          ,1, POSE_RUN),
        500,
      ],
      [
        (progress) =>
          moveTheft(
            50 + 140 * progress,
            240 + 100 * easeOutQuad(1 - alternateProgress(progress * 0.8))
          ),
        500,
      ],
      [
        (progress) =>
          moveTheft(
            190 + 120 * progress,
            320 + 300 * easeOutQuad(1 - alternateProgress(progress * 0.8))
          ),
        1000,
      ],
      [
        (progress) => {
          player.p.x = -100 + 390 * easeInQuad(progress);
          $backgroundV.$ = 4 + easeOutQuad(progress) * 2;
        },
        1000,
      ],
      [() => {
        effects.push(wipe());
        save(KEY_SAVE_NEED_TUTORIAL, 1);
      }],
      [() => {}, 1000],
    ],
  };

  let tempCamCenter;

  var stage2 = {
    [KEY_STAGE_INITIATE]() {
      player.p.x = -260;
      cameraCenter.y = player.p.y + 200;
      $cameraLoop.$ = () => {
        cameraCenter.y = Math.min(
          player.p.y - player.s.y / 2 + 200,
          Math.max(player.p.y + player.s.y / 2, cameraCenter.y)
        );
      };
      $backgroundColor.$ = '#ddeaf0';
      graphics.push(
        gradient(200, 400, 0, 0.4, [
          [0, '#ddeaf0'],
          [0.9, 'rgba(104,158,131,0.6)'],
        ]),
        gradient(
          3000,
          4000,
          1,
          0.6,
          [
            [0, '#ddeaf0'],
            [0.5, 'rgba(144,198,151,0.2)'],
            [1, 'rgba(221,234,240,0)'],
          ],
          true
        ),
        staticBamboo(330, -20, 2900, 1, 1.5, 51),
        staticBamboo(30, -10, 2900, 2, 1.2, 51),
        staticBamboo(0, 0, 2900, 3, 0.9, 10),
        staticBamboo(-67, 0, 3000, 5, 0.8, 10),
        staticBamboo(67, 60, 3100, 5, 0.7, 10)
      );
      platforms.push(
        ...boundarySet(),
        horizontalBamboo(-50, 150, 150),
        horizontalBamboo(120, 300, 200),
        horizontalBamboo(-120, 450, 200),
        horizontalBamboo(-50, 610, 180),
        verticalBamboo(-180, 800, 200),
        horizontalBamboo(50, 870, 150),
        verticalBamboo(150, 1100, 200),
        verticalBamboo(-100, 1400, 500),
        horizontalBamboo(220, 1350, 100),
        horizontalBamboo(150, 1620, 150),
        horizontalBamboo(-50, 1920, 150),
        horizontalBamboo(-220, 2220, 75),
        verticalBamboo(190, 2500, 500),
        horizontalBamboo(-136, 2900, 200)
      );
    },
    [KEY_STAGE_WAVES]: [
      () => {
        return [
          enemy('卡', 25, 790, [circularMovement(3000, 50, 5)], true),
          enemy('ㄚ', -20, 1150, [circularMovement(6000, 5, 10)], true),
          enemy('中', 150, 1580, [circularMovement(3000, 50, 5)], true),
          enemy('串', 75, 1800, [circularMovement(4000, 5, 50)], true),
          enemy('申', -120, 2220, [circularMovement(6000, 80, 80)], true),
          enemy('屮', -120, 2800, [circularMovement(6000, 5, 20)]),
        ]
      },
    ],
    [KEY_STAGE_IS_WAVE_CLEAN]() {
      const goalArea = object(-136, 2900, 200, 30);
      const collidedSide = collision(player, goalArea);
      return (
        $stageWave.$ === -1 ||
        (collidedSide && Math.abs(vectorMagnitude(player.v)) < 0.01)
      );
    },
    [KEY_STAGE_TRANSITION](progress) {
      player.p.x = -260 * easeInCirc(1 - progress);
    },
    [KEY_STAGE_ENDING_CUT_SCENE]: [
      [
        () => {
          $cameraLoop.$ = undefined;
          tempCamCenter = vector(cameraCenter.x, cameraCenter.y);
          $tempPlayerPos.$ = vector(player.p.x, player.p.y);
          graphics.push(...letterBox());
          $forceFacing.$ = 1;
        },
      ],
      [() => drawCaption('Still not found the theft.'), 500, true],
      [
        (progress) => {
          cameraCenter.y = lerp(tempCamCenter.y, player.p.y - 120, easeInOutQuad(progress));
        },
        2000,
      ],
      [summonTheft(-56, 2504, 3)],
      [
        (progress) =>
          moveTheft(-56 + 135 * progress, 2504 + 96 * easeOutQuad(progress)),
        500,
      ],
      [
        (progress) =>
          moveTheft(79 - 141 * progress, 2600 + 100 * easeOutQuad(progress), -1, POSE_RUN),
        500,
      ],
      [
        (progress) => {
          progress *= 1.4;
          moveTheft(
            -62 + 248 * progress,
            2700 + 100 * easeOutQuad(1 - alternateProgress(progress)),
            1, progress > 0.5 ? POSE_STOP : POSE_CHARGE
          );
        },
        1600,
      ],
      [
        (progress) => {
          $g.$ = 0;
          progress *= 1.6;
          player.p.x = lerp($tempPlayerPos.$.x, 180, progress);
          player.p.y = $tempPlayerPos.$.y + 50 * easeOutQuad(1 - alternateProgress(progress));
        },
        1000,
      ],
      [() => {
        effects.push(wipe());
        $forceFacing.$ = undefined;
      }],
      [() => {}, 1000],
    ],
  };

  var stage3 = {
    [KEY_STAGE_INITIATE]() {
      $titleY.$ = 300;
      $g.$ = 0.3;
      $maxReleaseVelocity.$ = 12;
      cameraCenter.y = player.p.y + 100;
      $reflectionY.$ = 0;
      $backgroundV.$ = 0.5;
      $backgroundColor.$ = '#D8DBE6';
      player.p.x = -DEFAULT_FRAME_WIDTH;
      $cameraLoop.$ = () => {
        cameraCenter.y = Math.max(
          player.p.y - player.s.y / 2 - 100,
          Math.min(100, cameraCenter.y)
        );
        $reflectionGradient.$ = createLinearGradient(0, 300, [
          [0, 'rgba(0,0,0,0)'],
          [0.01, 'rgb(117,137,160, 0.9)'],
          [0.9, '#2b435b'],
        ]);
      };
      graphics.push(
        gradient(1100, 1350, 10, 0.1, [
          [0, 'rgba(216,219,230,0)'],
          [0.1, 'rgba(109,130,152, 0)'],
          [0.5, 'rgb(109,130,152, 0.9)'],
          [1, '#2b435b'],
        ]),
        ...movingMountain(177, 0, 9, 0.3, 2.8),
        ...movingMountain(0, 40, 8, 0.2, 3.6),
        ...movingMountain(-37, 60, 8, 0.15, 4)
      );
      platforms.push(
        water(0, -200, DEFAULT_FRAME_WIDTH * 2, 400),
        ...boundarySet(-230)
      );
    },
    [KEY_STAGE_WAVES]: [
      () => [
        shell('亞', 75, 50, [
          slideIn(2500, 270, -40),
          circularMovement(7000, 0, 90, 2500),
        ]),
      ],
      () => [
        enemy('工', 10, 300, [
          firework(1, 4000, 1000, 0.25),
          slideIn(3500, 0, 550),
          circularMovement(8000, 100, 30, 3500),
        ]),
        shell('干', -120, 100, [
          slideIn(2500, -150, -250),
          // recover(3000, 3),
          circularMovement(5000, 10, 15, 2500),
        ]),
        shell('士', 120, 150, [
          // recover(3000, 3),
          slideIn(3500, 150, -250),
          circularMovement(6000, 10, 15, 3500),
        ]),
      ],
      () => {
        const core = enemy(
          '十',
          0,
          300,
          [
            slideIn(2300, 0, 550),
            circularMovement(
              10000,
              160,
              288,
              2300,
              (progress) => easeInOutQuad(alternateProgress(progress)) / -2
            ),
            (enemy) => {
              if (
                !enemy[KEY_ENEMY_DEAD_FRAME] &&
                children.filter((child) => child[KEY_ENEMY_IS_DEAD]).length ===
                  children.length
              ) {
                enemy[KEY_ENEMY_DEAD_FRAME] = enemy[KEY_OBJECT_FRAME];
              }
            },
          ],
          true
        );

        const children = [
          ['巛', vector(-50, 0)],
          ['三', vector(0, -50)],
          ['川', vector(50, 0)],
          ['二', vector(0, 50)],
        ].map(([appearance, offset], index) =>
          shell(appearance, offset.x, core.p.y + offset.y, [
            slideIn(
              2000 + index * 100,
              250 * (index > 1 ? 1 : -1),
              index % 2 === 1 ? 400 : 550
            ),
            follow(core, offset, 2300),
          ])
        );

        return [core, ...children];
      },
      () =>
        chain(
          shell('十', 0, 200, [
            // recover(3000, 3),
            slideIn(4000, 250, 450),
            firework(10, 6000, 1000),
            circularMovement(10000, 200, 210, 5000),
            // lemniscateMovement(12000, 500, 3000),
          ]),
          9,
          250,
          0,
          (i) => enemy(i === 0 ? '米' : '乂', 250, 450, [], i === 0)
        ),
      () =>
        chain(
          enemy(
            '回',
            0,
            300,
            [
              slideIn(5000, 270, -200),
              firework(10, 6000, 2000),
              circularMovement(
                20000,
                190,
                488,
                4000,
                (progress) => easeInOutQuad(alternateProgress(progress)) / -2
              )
            ],
            true
          ),
          9,
          250,
          1,
          (i) =>
            (i === 0 ? shell : enemy)(
              i === 0 ? '由' : i % 2 == 1 ? '口' : '回',
              270,
              -200,
              // [...(i === 0 ? [recover(2500, 3)] : [])],
              [],
              i === 8
            )
        ),
    ],
    [KEY_STAGE_IS_WAVE_CLEAN]() {
      return enemies.length === 0 && player.p.y <= player.s.y / 2;
    },
    [KEY_STAGE_TRANSITION](progress) {
      const movementProgress = 1 - easeInOutQuad(alternateProgress(progress));
      $cameraZoom.$ = 1 - movementProgress * 0.1;
      $backgroundV.$ = 1 + movementProgress * 3;
      player.v.y = 0;
      player.p.y =
        (1 - easeInQuad(alternateProgress(progress))) * 200 + player.s.y / 2;
      if (progress == 0) $tempPlayerPos.$ = vector(player.p.x, player.p.y);
      else player.p.x = $tempPlayerPos.$.x * easeInOutQuad(1 - progress);
    },
    [KEY_STAGE_ENDING_CUT_SCENE]: [
      [
        () => {
          $g.$ = 0;
          player.v.y = 0;
          graphics.push(...letterBox());
          $tempPlayerPos.$ = vector(player.p.x, player.p.y);
          $forceFacing.$ = 1;
          const offset = player[KEY_OBJECT_FRAME];
          player[KEY_OBJECT_ON_UPDATE].push(
            objectAction(
              2000,
              (player, progress) => {
                player.p.y = player.s.y / 4 + 140 * easeOutQuad(1 - alternateProgress(progress));
                player.p.x =
                  $tempPlayerPos.$.x +
                  (-100 - $tempPlayerPos.$.x) * easeInOutQuad(progress);
                if (Math.round(player.p.x) === -100)
                  $tempPlayerPos.$ = vector(player.p.x, player.p.y);
              },
              {
                [KEY_OBJECT_EVENT_GET_OFFSET]: () => offset,
              }
            )
          );
        },
      ],
      [
        (progress) => {
          $backgroundV.$ = 1 + easeOutQuad(progress) * 2;
        },
        2000,
      ],
      [() => drawCaption('Still not found the theft.'), 500, true],
      [summonTheft(-300, 0, 11)],
      [
        (progress) =>
          moveTheft(-300 + 100 * progress, 200 - 200 * easeInQuad(progress), 1, POSE_STOP),
        1000,
      ],
      [
        (progress) =>
          moveTheft(
            -200 + 100 * progress,
            100 * easeOutQuad(1 - alternateProgress(progress))
          ),
        800,
      ],
      [
        (progress) =>
          moveTheft(
            -100 + 200 * progress,
            100 * easeOutQuad(1 - alternateProgress(progress)),
            1, progress > 0.5 ? POSE_STOP : POSE_CHARGE
          ),
        800,
      ],
      [
        (progress) =>
          moveTheft(
            100 + 300 * progress,
            300 * easeOutQuad(1 - alternateProgress(progress))
          ),
        1200,
      ],
      [
        () => {
          player[KEY_OBJECT_ON_UPDATE].pop();
          $tempPlayerPos.$ = vector(player.p.x, player.p.y);
        },
      ],
      [
        (progress) => {
          player.p.y =
            $tempPlayerPos.$.y +
            100 * easeOutQuad(1 - alternateProgress(progress * 2));
          player.p.x = -100 + 500 * easeOutQuad(progress);
          $backgroundV.$ = 4 + easeOutQuad(progress) * 5;
        },
        1800,
      ],
      [() => {
        effects.push(wipe());
        $forceFacing.$ = undefined;
      }],
      [() => {}, 1000],
    ],
  };

  const cliffPaths = [
    [-51, -50, SIDE_L, 1.81, decompressPath(`¢gge|l	oodo~''$#'$'`)],
    [316, 613, SIDE_R, 1.93, decompressPath(`'¦%#nm	$'eeggt~eg`)],
    [289, 1229, SIDE_R, 1.93, decompressPath(`.{}'..#ggggggg`), [20, 23]],
    [-6, 1513, SIDE_L, 1.93, decompressPath(`Qi{tl'3G;D=GOGE`), [13]],
    [326, 1851, SIDE_R, 1.8, decompressPath(`8m''''/'6GFwr~gog~yogi`)],
    [-34, 2775, SIDE_L, 2.08, decompressPath(`6grgo''''/`)],
  ];

  const generateCiff = ([x, y, side, scale, image, switchSideIndex = []], i) => {
    const getPathPointPos = (p) =>
      vector((x + p.x) * scale, (y + p.y + image.h / 2) * scale);

    graphics.push(
      graphic(x, y, (graphic) =>
        draw(11, (ctx) => {
          const _getPathPointPos = (p) =>
            vector(
              (graphic.p.x + p.x) * scale,
              (graphic.p.y + p.y + image.h / 2) * scale
            );

          ctx.fillStyle = '#415061';
          ctx.beginPath();
          image.p.forEach((p) => {
            ctx.lineTo(...transform(_getPathPointPos(p)));
          });
          ctx.fill();
        })
      )
    );

    switchSideIndex = switchSideIndex.slice();
    image.p.slice(1, image.p.length).forEach((p, index) => {
      if (switchSideIndex[0] === index) {
        switchSideIndex.shift();
        side = side === SIDE_R ? SIDE_L : SIDE_R;
      }

      const p1 = getPathPointPos(p);
      const p2 = getPathPointPos(image.p[index]);
      const xDiff = p2.x - p1.x;
      const yDiff = p1.y - p2.y;
      const w = Math.abs(xDiff);
      const h = Math.abs(yDiff);

      if (w > player.s.x / 2) {
        platforms.push(
          platform(
            (xDiff > 0 ? p1.x : p2.x) + w / 2,
            side == SIDE_L ? (xDiff < 0 ? p1.y : p2.y) : xDiff < 0 ? p2.y : p1.y,
            w - 2,
            1
          )
        );
      }
      if (h > player.s.y / 2) {
        platforms.push(
          platform(
            side == SIDE_L ? (xDiff > 0 ? p1.x : p2.x) : xDiff > 0 ? p2.x : p1.x,
            (yDiff > 0 ? p1.y : p2.y) - h / 2,
            1,
            h
          )
        );
      }
    });
  };

  const _randomMovement = () => [
    circularMovement(
      Math.random() * 1000 + 2000,
      Math.random() * 10,
      Math.random() * 10
    ),
  ];

  let tempCamCenter$1;
  const fakeMountain = staticMountain(205, 2750, 7, 1, 2);

  var stage4 = {
    [KEY_STAGE_INITIATE]() {
      $titleY.$ = 580;
      $backgroundColor.$ = '#D8DBE6';
      $reflectionY.$ = 0;
      player.p.x = -250;
      player.p.y = 400;
      $g.$ = 0;
      cameraCenter.y = player.p.y + 200;
      $cameraLoop.$ = () => {
        $reflectionGradient.$ = createLinearGradient(0, 300, [
          [0, 'rgba(0,0,0,0)'],
          [0.01, 'rgb(117,137,160, 0.9)'],
          [0.9, '#2b435b'],
        ]);
        cameraCenter.y = Math.max(
          player.p.y - player.s.y / 2,
          Math.min(200, cameraCenter.y)
        );
      };

      cliffPaths.forEach(generateCiff);

      platforms.push(
        ...boundarySet(-12),
        water(0, -24, DEFAULT_FRAME_WIDTH * 2, 50),
        flow(-55, 1592.5, 40, 1235, vector(0, -0.5), 30),
        flow(90, 2225, 270, 30, vector(-0.2, 0), 10)
      );

      const cloudGradient = [
        [0, 'rgba(255,255,255,0)'],
        [0.5, 'rgba(255,255,255,0.9)'],
        [1, 'rgba(255,255,255,0)'],
      ];
      graphics.push(
        gradient(1100, 1350, 10, 0.1, [
          [0, 'rgba(216,219,230,0)'],
          [0.1, 'rgba(109,130,152, 0)'],
          [0.5, 'rgb(109,130,152, 0.9)'],
          [1, '#2b435b'],
        ]),
        gradient(
          3000,
          14000,
          9,
          0.03,
          [
            [0, 'rgba(216,219,230,0)'],
            [1, 'rgba(216,219,230,1)'],
          ],
          1
        ),
        gradient(
          1200,
          6550,
          1,
          0.06,
          [
            [0, 'rgba(216,219,230,1)'],
            [1, 'rgba(43,67,91,0)'],
          ],
          1
        ),
        graphic(
          0,
          0,
          () =>
            draw(10, (ctx) => {
              ctx.fillStyle = '#000';
              ctx.fillRect(
                ...transform(vector(-232, 2)),
                transform(76),
                transform(140)
              );
            }),
          [
            objectEvent(() => {
              if (Math.random() > 0.5) effects.push(ripple(-190, 0, 300));
            }, 1000),
          ]
        ),
        graphic(0, 0, () =>
          draw(10, (ctx) => {
            ctx.fillStyle = 'rgba(255,255,255,0.6)';
            ctx.beginPath();
            ctx.moveTo(...transform(vector(-45, 2210)));
            ctx.arc(
              ...transform(vector(-45, 2210)),
              transform(30),
              Math.PI,
              -Math.PI / 2
            );
            ctx.fill();
          })
        ),
        gradient(5421, 250, 51, 1.5, cloudGradient, 1),
        gradient(5421, 250, 8, 0.5, cloudGradient, 1),
        gradient(5421, 250, 8, 0.3, cloudGradient, 1),
        // staticMountain(-100, -42, 7, 0.6, 5),
        staticMountain(177, 0, 7, 0.5, 3),
        // staticMountain(177, 0, 7, 0.3, 2.8),
        staticMountain(-50, 40, 7, 0.2, 3.6),
        // staticMountain(-237, 40, 7, 0.17, 3.5),
        staticMountain(-266, 40, 7, 0.15, 5),
        staticMountain(226, 100, 7, 0.12, 5),
        // staticMountain(16, 100, 7, 0.1, 4),
        staticMountain(-406, 110, 7, 0.075, 4),
        staticMountain(16, 110, 7, 0.06, 4)
      );
    },
    [KEY_STAGE_WAVES]: [
      () => [
        ...compund(
          enemy('巾', -160, 3790, [circularMovement(2500, 40, 5)], 1),
          ...[
            ['亓', 0, 4340],
            ['冂', -130, 4516],
            ['兀', -61, 4694],
            ['ㄢ', 4, 4850],
            ['只', 40, 5032],
            ['天', 124, 5194],
            ['云', -49, 5319],
            ['皿', 49, 5444],
            ['弓', 27, 5611],
            ['六', -76, 5740],
            ['廾', -141, 1596],
            ['爪', -119, 2127],
            ['孓', -173, 2927],
          ].map(([appearance, x, y]) => enemy(appearance, x, y, _randomMovement()))
        ),
        shell('卞', -179, 3409, [circularMovement(3000, 20, 5)]),
        enemy('卝', 143, 750, _randomMovement()),
        enemy('从', 80, 1509, _randomMovement(), 1),
        ...chain(
          enemy(
            '公',
            -300,
            2767,
            [
              objectAction(5000, (enemy, progress) => {
                progress = easeInOutQuad(alternateProgress(progress));
                enemy.p.y = enemy[KEY_OBJECT_INITIAL_POS].y + 452 * progress;
                enemy.p.x =
                  enemy[KEY_OBJECT_INITIAL_POS].x +
                  400 * Math.sin((easeOutCirc(progress) * Math.PI) / 2);
              }),
            ],
            1
          ),
          10,
          300,
          8,
          (i, head) => (i === 7 ? shell : enemy)('ㄙ', head.p.x, head.p.y, [], 1)
        ),
      ],
    ],
    [KEY_STAGE_IS_WAVE_CLEAN]() {
      const goalArea = object(-131, 6226, 200, 80);
      const collidedSide = collision(player, goalArea);
      return (
        $stageWave.$ === -1 ||
        (collidedSide && Math.round(vectorMagnitude(player.v)) === 0)
      );
    },
    [KEY_STAGE_TRANSITION](progress) {
      $g.$ = G;
      player.v.y = 0;
      player.p.y = (1 - easeInQuad(progress)) * 400;
      player.p.x = -1600 * easeInQuad(1 - progress);
    },
    [KEY_STAGE_ENDING_CUT_SCENE]: [
      [
        () => {
          $forceFacing.$ = 1;
          $cameraLoop.$ = undefined;
          $tempPlayerPos.$ = vector(player.p.x, player.p.y);
          tempCamCenter$1 = vector(cameraCenter.x, cameraCenter.y);
          graphics.push(
            ...letterBox(),
            fakeMountain
          );
        }
      ],
      [
        (progress) => {
          cameraCenter.y = lerp(tempCamCenter$1.y, player.p.y - 30, easeInOutQuad(progress));
        },
        1000,
      ],
      [() => {
        summonTheft(310, 6310, 11)();
        moveTheft(310, 6310, -1, POSE_IDLE);
      }],
      [() => drawCaption("Still can't find him."), 500, true],
      [
        (progress) => {
          cameraCenter.x = lerp(tempCamCenter$1.x, 130, easeInOutQuad(progress));
          player.p.x = lerp($tempPlayerPos.$.x, -60, easeInOutQuad(progress));
        },
        2000,
      ],
      [() => {}, 1000],
      [
        progress =>
          moveTheft(lerp(305, 450, easeOutQuint(progress)), lerp(6310, 6410, easeOutQuint(progress)), 1, POSE_CHARGE)
        ,1000
      ],
      [
        (progress) => {
          $g.$ = 0;
          draw(0, ctx => ctx.canvas.style.filter = `saturate(${1 - progress})`);
          player.p.x = lerp(-60, 134, easeOutQuint(progress));
          player.p.y = lerp($tempPlayerPos.$.y, 6360, easeOutQuint(progress));
        },
        1000,
      ],
      [() => drawCaption('(To be continued)'), 500, true],
      [() => {
        effects.push(wipe());
      }],
      [() => {}, 1000],
      [() => {
        $forceFacing.$ = undefined;
        draw(0, ctx => ctx.canvas.style.filter = '');
      }],
    ]
  };

  var stages = [
    stage1,
    stage2,
    stage3,
    stage4
  ];

  let initialWave = +load(KEY_SAVE_WAVE) || 0;

  const creatStage = (config) => ({
    ...object(),
    ...config,
    [KEY_OBJECT_ON_UPDATE]: [
      stage => {
        if ($stageWave.$ === -1 && $stageNextWave.$ !== initialWave) {
          if($isGameStarted.$) setWave(initialWave);
        } else if ($stageWave.$ === stage[KEY_STAGE_WAVES].length) {
          const [callback, duration = FRAME_DURAITON, wait] = stage[
            KEY_STAGE_ENDING_CUT_SCENE
          ][stage[KEY_STAGE_ENDING_CUT_SCENE_INDEX]];
          const frameDiff =
            stage[KEY_OBJECT_FRAME] - stage[KEY_STAGE_ENDING_CUT_SCENE_FRAME];
          if (frameDiff >= Math.round(duration / FRAME_DURAITON)) {
            stage[KEY_STAGE_ENDING_CUT_SCENE_FRAME] = stage[KEY_OBJECT_FRAME];
            const nextAnimation = () => {
              if (
                stage[KEY_STAGE_ENDING_CUT_SCENE_INDEX] <
                stage[KEY_STAGE_ENDING_CUT_SCENE].length - 1
              ) {
                stage[KEY_STAGE_ENDING_CUT_SCENE_FRAME] = stage[KEY_OBJECT_FRAME];
                stage[KEY_STAGE_ENDING_CUT_SCENE_INDEX]++;
                stage[KEY_STAGE_ENDING_CUT_SCENE][
                  stage[KEY_STAGE_ENDING_CUT_SCENE_INDEX]
                ][0](0);
              } else {
                save(KEY_SAVE_WAVE, undefined);
                initialWave = 0;
                const nextStage = ($stageIndex.$ + 1) % stages.length;
                if(nextStage === 0) $isGameStarted.$ = false;
                setStage(nextStage);
              }
            };
            if (wait) {
              waitForClick(
                `${KEY_STAGE_ENDING_CUT_SCENE_KEY}${$stageIndex.$}${stage[KEY_STAGE_ENDING_CUT_SCENE_INDEX]}`,
                nextAnimation
              );
            } else {
              nextAnimation();
            }
          } else {
            callback(getActionProgress(frameDiff, duration));
          }
        } else if (stage[KEY_STAGE_TRANSITION_FRAME] !== undefined) {
          const progress = getActionProgress(
            stage[KEY_OBJECT_FRAME] - stage[KEY_STAGE_TRANSITION_FRAME],
            STAGE_TRANSITION_DURAION,
            false
          );
          if (stage[KEY_STAGE_TRANSITION]) stage[KEY_STAGE_TRANSITION](Math.max(0, Math.min(1, progress)));
        } else {
          if (
            !player[KEY_PLAYER_DEATH_FRAME] &&
            stage[KEY_STAGE_IS_WAVE_CLEAN] &&
            stage[KEY_STAGE_IS_WAVE_CLEAN]()
          ) {
            ($stageWave.$ === stage[KEY_STAGE_WAVES].length - 1 ? _setWave : setWave)(
              $stageWave.$ + 1
            );
          }
        }
      },
      objectEvent(
        () => _setWave($stageNextWave.$),
        STAGE_TRANSITION_DURAION,
        {
          [KEY_OBJECT_EVENT_GET_OFFSET]: (stage) =>
            stage[
              stage[KEY_STAGE_TRANSITION_FRAME] === undefined
                ? KEY_OBJECT_FRAME
                : KEY_STAGE_TRANSITION_FRAME
            ] - 1,
          [KEY_OBJECT_EVENT_FIRST_FRAME_TRIGGER]: true
        }
      )
    ],
    [KEY_STAGE_ENDING_CUT_SCENE_INDEX]: 0,
    [KEY_STAGE_ENDING_CUT_SCENE_FRAME]: 0,
    [KEY_STAGE_ENDING_CUT_SCENE]: [
      [() => {}, 0],
      ...(config[KEY_STAGE_ENDING_CUT_SCENE] || []),
    ],
  });

  function setWave(wave) {
    $stageNextWave.$ = wave;
    $stage.$[KEY_STAGE_TRANSITION_FRAME] = $stage.$[KEY_OBJECT_FRAME];
    if ($stage.$[KEY_STAGE_TRANSITION]) $stage.$[KEY_STAGE_TRANSITION](0);
  }

  function _setWave(wave) {
    delete $stage.$[KEY_STAGE_TRANSITION_FRAME];
    enemies.splice(0, enemies.length);
    $stageWave.$ = wave;
    if(wave !== -1 && wave !== $stage.$[KEY_STAGE_WAVES].length) save(KEY_SAVE_WAVE, wave);
    if ($stage.$[KEY_STAGE_WAVES][wave])
      enemies.push(...$stage.$[KEY_STAGE_WAVES][wave]());
  }

  function setStage(stageIndex, wave) {
    vectorOp(() => 0, [], player.p);
    vectorOp(() => 0, [], player.v);
    vectorOp(() => 0, [], cameraCenter);
    revive();
    $g.$ = G;
    $maxReleaseVelocity.$ = MAX_RELEASE_VELOCITY;
    $reflectionY.$ = undefined;
    $backgroundV.$ = 0;
    $backgroundColor.$ = undefined;
    $reflectionGradient.$ = undefined;
    enemies.splice(0, enemies.length);
    platforms.splice(0, platforms.length);
    projectiles.splice(0, projectiles.length);
    graphics.splice(0, graphics.length);
    if (stageIndex < stages.length) {
      $stageIndex.$ = stageIndex;
      $stageNextWave.$ = -1;
      $stage.$ = creatStage(stages[stageIndex]);
      if (wave) setWave(wave);
      else $stageWave.$ = -1;
      $stage.$[KEY_STAGE_INITIATE]();
      save(KEY_SAVE_STAGE, stageIndex);
    }
    if(!$isGameStarted.$) {
      let startFrame;
      const title = graphic(0,0,() => {
        let opacity;
        if(startFrame != undefined) {
          const progress = getActionProgress($stage.$[KEY_OBJECT_FRAME] - startFrame, 1000, false);
          if(progress >= 1) return graphics.splice(graphics.indexOf(title), 1);
          opacity = 1 - progress;
        } else {
          const progress = Math.min(1, getActionProgress($stage.$[KEY_OBJECT_FRAME], 2000, false));
          if(progress >= 1) {
            waitForClick(KEY_GAME_START_KEY, () => {
              playHarmony();
              $isGameStarted.$ = true;
              startFrame = $stage.$[KEY_OBJECT_FRAME];
            });
          }
          opacity = progress;
        }
        drawTitle(opacity);
      });
      graphics.push(title);
    }
  }

  setStage(+load(KEY_SAVE_STAGE) || 0);

  // window.addEventListener('keydown', ({ key }) => {
  //   if (key === 'Shift') setStage(($stageIndex.$ + 1) % stages.length);
  //   if (key === 'Control')
  //     _setWave(($stageWave.$ + 1) % $stage.$[KEY_STAGE_WAVES].length);
  // });

  const defaultColor = ['#666', '#111', '#c4c4c4', '#333', '#888'];

  let facing = 1;
  const skeleton = createSkeletion();

  function setAngle(index, angle) {
    skeleton.j[index][1] = angle;
  }

  function animateToPose(frameKey, duration, from, to, timing, removeKey = 1) {
    const progress = getActionProgress(player[KEY_OBJECT_FRAME] - player[frameKey], duration, false);
    if(progress > 1 || !player[frameKey]) {
      skeleton.p(to);
      if(removeKey) player[frameKey] = undefined;
    } else {
      skeleton.p(from.map((angle, index) => lerp(angle, to[index], timing(progress))));
    }
  }

  player[KEY_OBJECT_ON_UPDATE] = [
    // check death
    objectEvent(
      () => setStage($stageIndex.$, $stageWave.$),
      PLAYER_DEATH_ANIMATION_DURATION,
      { [KEY_OBJECT_EVENT_GET_OFFSET]: (stage) => stage[KEY_PLAYER_DEATH_FRAME] }
    ),
    // update properties
    () => {
      // check death
      if (!player[KEY_PLAYER_DEATH_FRAME] && $health.$ === 0) {
        playSound(1);
        player[KEY_PLAYER_DEATH_FRAME] = player[KEY_OBJECT_FRAME];
        effects.push(wipe());
      }
    
      // gravity pulling
      player.v.y -= $g.$ * $timeRatio.$;
    
      // update position
      vectorOp((pos, v) => pos + v * $timeRatio.$, [player.p, player.v], player.p);
      if($cameraLoop.$) $cameraLoop.$();
      
      // update facing
      const vFacing = player.v.x / Math.abs(player.v.x) || 1;
      if($forceFacing.$) {
        facing = $forceFacing.$;
      } else if($isPressing.$) {
        const { x } = getReleaseVelocity();
        facing = x / Math.abs(x) || 1;
      } else if(player[KEY_PLAYER_DEATH_FRAME] || player[KEY_PLAYER_CHARGE_FRAME] < player[KEY_PLAYER_DAMAGE_FRAME]) {
        facing = -vFacing;
      } else if($playerCollisionSide.$[SIDE_R]) {
        facing = 1;
      } else if($playerCollisionSide.$[SIDE_L]) {
        facing = -1;
      } else {
        facing = vFacing;
      }
    
      // update pose
      skeleton.j[0][0].y = 9;
      if(player[KEY_PLAYER_DEATH_FRAME]) {
        // die
        skeleton.j[0][0].y = -25;
        animateToPose(KEY_PLAYER_DEATH_FRAME, 1000, POSE_DAMAGED, POSE_DIE, easeOutQuint, false);
      } else if(player[KEY_PLAYER_ATTACK_FRAME]) {
        // attack
        animateToPose(KEY_PLAYER_ATTACK_FRAME, 300, POSE_ATTACK, POSE_CHARGE, easeInQuint);
      } else if(player.p.y < $reflectionY.$) {
        // swim
        skeleton.p(POSE_SWIM);
        const progress = getActionProgress(player[KEY_OBJECT_FRAME], 600);
        const p = easeInOutQuad(alternateProgress(progress));
        setAngle(2, lerp(POSE_SWIM[2], 0.032, p));
        setAngle(3, lerp(POSE_SWIM[3], 0.2, p));
        setAngle(6, lerp(POSE_SWIM[6], 0.246, alternateProgress(easeInQuad(progress))));
        setAngle(7, lerp(POSE_SWIM[7], 0.134, alternateProgress(easeInQuad(progress))));
      } else if($playerCollisionSide.$[SIDE_T]) {
        if(vectorMagnitude(player.v) <= 0.6) {
          if($backgroundV.$ > 0) {
            // run
            if(player[KEY_PLAYER_STOP_FRAME]) {
              animateToPose(KEY_PLAYER_STOP_FRAME, 200, POSE_STOP, POSE_RUN, easeInQuint);
            } else {
              if($isPressing.$) skeleton.p(POSE_CHARGE);
              else skeleton.p(POSE_RUN);
              const progress = getActionProgress(player[KEY_OBJECT_FRAME], 600);
              const p = easeInOutQuad(alternateProgress(progress));
              skeleton.j[0][0].y = 8 + 2 * alternateProgress(p);
              setAngle(2, lerp(POSE_RUN[2], -0.16, p));
              setAngle(3, lerp(POSE_RUN[3], 0.172, p));
              setAngle(6, lerp(POSE_RUN[6], 0.2, alternateProgress(easeInQuad(progress))));
              setAngle(7, lerp(0.2, 0, alternateProgress(easeInQuad(progress))));
            }
            facing = 1;
          } else {
            // idle
            skeleton.j[0][0].y = 11.9;
            animateToPose(KEY_PLAYER_STOP_FRAME, 200, POSE_STOP, POSE_IDLE, easeOutQuint);
          }
        } else {
          // stopping
          player[KEY_PLAYER_STOP_FRAME] = player[KEY_OBJECT_FRAME];
          skeleton.p(POSE_STOP);
        }
      } else if(player[KEY_PLAYER_CHARGE_FRAME] < player[KEY_PLAYER_DAMAGE_FRAME] && isPlayerInvincibleAfterDamage()) {
        skeleton.p(POSE_DAMAGED);
      } else {
        skeleton.p(POSE_CHARGE);
      }
      defaultColor[2] = ($health.$ == 2 ? '#c4c4c4' : '#ec5751');
    }, 
    // rendering
    () => {
      draw(25, ctx => {
        // draw trajectory
        if ($isPressing.$ && isAbleToDash()) {
          ctx.lineCap = 'round';
          ctx.strokeStyle = `rgba(0,0,0,${isReleaseVelocityEnough() ? 0.1 : 0})`;
          const line = 10;
          for(let i = 0; i < line; i++) {
            ctx.lineWidth = transform(Math.random() * 3 + 1);
            ctx.beginPath();
            ctx.moveTo(...transform(player.p));
            const path = playerTrajectory();
            for(let j = 0; j < Math.random() * path.length / 2 + path.length / 2; j++) {
              const dashLength = transform(50);
              ctx.setLineDash([Math.random() * dashLength * 2 + dashLength, Math.random() * dashLength]);
              ctx.lineTo(...transform(vector(
                path[j].x + i / 1 * Math.cos(i / line * 2 * Math.PI),
                path[j].y + i / 1 * Math.sin(i / line * 2 * Math.PI + 10)
              )));
            }
            ctx.stroke();
          }
          ctx.lineCap = 'butt';
          ctx.setLineDash([]);
        }
        
        // draw character
        if(isPlayerInvincibleAfterDamage()) {
          ctx.globalAlpha = Math.round(player[KEY_OBJECT_FRAME]) % 8 > 3 ? 0.1 : 1;
        }
        skeleton.d(ctx, player.p, defaultColor, facing);
        ctx.globalAlpha = 1;
    
        // water mask
        if (isUnderWater(player)) {
          skeleton.d(ctx, player.p, defaultColor.map(() => $reflectionGradient.$), facing);
        }
    
        // reflection
        if ($reflectionY.$ !== undefined) {
          ctx.globalAlpha = 0.3 * easeInQuad(Math.max(0, Math.min(1, player.p.y / player.s.y * 2)));
          skeleton.d(ctx, vector(
            player.p.x - Math.random() * 4 * $timeRatio.$,
            -player.p.y
          ), defaultColor, facing, -1);
          ctx.globalAlpha = 1;
        }
      });
    },
    checkRipple()
  ];

  var modules = [
    background$1,
    time,
    objects,
    interaction,
  ];

  const canvas = document.querySelector("canvas");
  const ctx = canvas.getContext("2d");

  let devicePixelRatio;

  function resize() {
    devicePixelRatio = window.devicePixelRatio;
    let vw = window.innerWidth;
    let vh = window.innerHeight;
    if(vw < vh / ASPECT_RATIO) vh = vw * ASPECT_RATIO;
    if(vh < vw * ASPECT_RATIO) vw = vh / ASPECT_RATIO;
    canvas.style.width = Math.floor(vw);
    canvas.style.height = Math.floor(vh);
    canvas.width = Math.floor(vw * devicePixelRatio);
    canvas.height = Math.floor(vh * devicePixelRatio);
    ctx.textBaseline = 'middle';
    ctx.textAlign = 'center';
    ctx.scale(devicePixelRatio, devicePixelRatio);
    $canvasLeftOffset.$ = canvas.getBoundingClientRect().left;
    cameraFrameSize.x = Math.floor(canvas.width / devicePixelRatio);
    cameraFrameSize.y = Math.floor(canvas.height / devicePixelRatio);
  }

  window.addEventListener('resize', () => setTimeout(resize, FRAME_DURAITON));
  resize();

  let lastTimeTick;
  function tick() { 
    if(devicePixelRatio != window.devicePixelRatio) resize();
    if(!lastTimeTick || (Date.now() - lastTimeTick) >= FRAME_DURAITON / 2) {
      modules.map(render => render(ctx));
      lastTimeTick = Date.now();
    }
    // if(!$debug.$) 
    requestAnimationFrame(tick);
  }tick();


  // window.addEventListener('keydown', ({ key }) => {
  //   if(key === '`') {
  //     $debug.$ = true;
  //     tick();
  //   }
  //   if(key === 'Backspace') {
  //     $debug.$ = false;
  //     tick();
  //   }
  // });

}());
