let gui;

// #region boolCheck
playing = false;
rverbPlaying = false;
delayPlaying = false;
let distPlaying = false;
activateFilter = false;
activateRate = false;
recording = false;
// #endregion boolCheck

// #region Reverb
let reverb;
let x = 0;
let y = 0;
let z =0;
// #endregion Reverb

// #region Delay
let delay;
let dt;
let feedback;
// #endregion Delay

let rec;
let sound;
let fft;

//#region Equalizer
let filterFreq;
let res;
//#endregion Equalizer

//#region Distortion
let distortion;
let amount;
let over; // string none, 2x or 4x
//#endregion Distortion

function preload()
{
  soundFormats("wav");
  sound = loadSound("s.wav");
}
function setup() 
{
  playing = false;
  rverbPlaying = false;
  distPlaying = false;
  delayPlaying = false;
  activateFilter = false;
  activateRate = false;
  recording = false;

  InstancingEffects();
  createCanvas(windowWidth,windowHeight);
 
  gui = createGui();
  gui.loadStyle("blue");
  Buttons();
  noStroke();
  fill(82, 138, 227); 
}
function draw() 
{
  background(0);
  fill(125);
  stroke('red');
  strokeWeight(4);
  rect(0, windowHeight -170, windowWidth, 168 );
  drawGui();
  Texts();
  music();
  sound.connect(filter);
  fill('green'); 
  FFTANALYZER();
}
// Mixer
function music()
{
  // Play //
  if(playing == false)
  {
    if(a.isPressed && playing == false)
    {
      playing = true;
      sound.loop();
    }
  }
  // Stop //
  if (b.isPressed && playing == true)
  {
    sound.stop();
    playing = false;
  }
  //Verb Button
  if (c.isPressed && rverbPlaying == false && playing == true )
  {
    Verb();
  }
  else if (c.isPressed && rverbPlaying == true && playing == true) 
  {
    DiscVerb();
  }
  // Dist Button
  if(g.isPressed && playing == true && distPlaying == false)
  {
    Dist();
  }
  else if (g.isPressed && playing == true && distPlaying == true)
  {
    QuitDistortion();
  }
  // Delay Buttons
  if(del.isPressed && playing == true && delayPlaying == false)
  {
    let newDt;
    let newFeed;
    if (dell.isChanged)
    {
      newDt = dell.val;
      dell.val = map(0,1,0,2);
    } 
    if (i.isChanged)
    {
      newFeed = i.val;
      i.vall = map(0,1,0,3);
    }
    HandleDealy(dell.val, i.val);
  }
  else if (del.isPressed && playing == true && delayPlaying == true)
  {
    QuitDelay();
  }
  // Filter Buttons
  if (k.isPressed && playing == true && activateFilter == false)
  {
    HIghPass();
  }
  else if (k.isPressed && playing == true && activateFilter == true)
  {
    QuiutGilter();
  }
  // Rate Buttons
  if (rateBtn.isChanged)
  {
    newRate = rateBtn.val;
    newRate = map(newRate,0,1,-1,2);
    sound.rate(newRate);
  }
  // Rec BTN
  if(recBtn.isPressed && recording == false)
  {
    rec.record();
    recording = true;
  }
  else if (recBtn.isPressed && recording == true)
  {
    rec.stop();
    recording = false;
    sound.stop();
    save(sound, 'mySound.wav');
  }
}
// Buttons
function Buttons()
{
  a = createToggle("Play", windowWidth -200,windowHeight - 150);
  a.setStyle(
  {
    fillBgOn: color("#1461de"),
    fillBgOff: color("#1417de"),
    fillBgHover : color("#378cdb"),
    fillBgActive: color("#37addb"),
    fillLabel: color("#de7a10"),
    fillLabelHover: color("#de8f10"),
    fillLabelOnActive: color("#deae10"),
    strokeBgOn: color("#dec310"),
    rounding: 20,
    textSize: 20
  }
  );
  b = createToggle("Stop", windowWidth -200,windowHeight - 100);
  b.setStyle(
  {
    fillBgOn: color("#ed0707"),
    fillBgOff: color("#8a0606"),
    fillBgHover : color("#8f0606"),
    fillBgActive: color("#ed0707"),
    fillLabel: color("#000000"),
    fillLabelHover: color("#000000"),
    fillLabelActive: color("#000000"),
    strokeBgOn: color("#912f19"),
    rounding: 20,
    textSize: 20
  }
  );
  //Reverb
  c = createToggle("Reverb",windowWidth - 400 ,windowHeight - 150);
  c.setStyle(
  {
    fillBgOn: color("#70fdff"),
    fillBgOff: color("#339d9e"),
    fillBgHover : color("#56eef0"),
    fillBgActive: color("#20e5e8"),
    fillLabel: color("#000000"),
    fillLabelHover: color("#000000"),
    fillLabelActive: color("#000000"),
    strokeBgOn: color("#e89f20"),
    rounding: 20,
    textSize: 20
  });
  d = createCheckbox("Reverse", windowWidth - 275,windowHeight -100);
  d.setStyle(
  {
    fillBg: color("#70fdff"),
     fillBgHover : color("#56eef0"),
     fillBgActive: color("#20e5e8"),
     fillCheck: color("#000000"),
     fillCheckHover: color("#000000"),
     fillCheckActive: color("##e89f20"),
     strokeBg: color("#e89f20"),
     rounding: 20
  });
  e = createSlider("Delay Time", windowWidth - 400,windowHeight -100,100,32);
  e.setStyle(
  {
     fillBg: color("#70fdff"),
     fillBgHover : color("#56eef0"),
     fillBgActive: color("#20e5e8"),
     fillTrack: color("#e89f20"),
     fillTrackHover: color("#000000"),
     fillTrackActive: color("##e89f20"),
     strokeBg: color("#e89f20"),
     rounding: 20,
     textSize: 20
  });
  f = createSlider("Decay Rate", windowWidth - 400,windowHeight -50,100,32);
  f.setStyle(
  {
     fillBg: color("#70fdff"),
     fillBgHover : color("#56eef0"),
     fillBgActive: color("#20e5e8"),
     fillTrack: color("#e89f20"),
     fillTrackHover: color("#e89f20"),
     fillTrackActive: color("#e89f20"),
     strokeBg: color("#e89f20"),
     rounding: 20,
     textSize: 20
  });
  // Delay
  del = createToggle("Delay", windowWidth - 600,windowHeight - 150);
  del.setStyle(
  {
    fillBgOn: color("#1cbd37"),
    fillBgOff: color("#0f7520"),
    fillBgHover : color("#2bbd44"),
    fillBgActive: color("#37bd4e"),
    fillLabel: color("#000000"),
    fillLabelHover: color("#000000"),
    fillLabelActive: color("#000000"),
    strokeBgOn: color("#b03f0e"),
    rounding: 20,
    textSize: 20
  });
  dell = createSlider("Delay Time", windowWidth - 600,windowHeight -100,100,32);
  dell.setStyle(
  {
     fillBg: color("#1cbd37"),
     fillBgHover : color("#32c74b"),
     fillBgActive: color("#42c758"),
     fillTrack: color("#bd4c1a"),
     fillTrackHover: color("#c75f32"),
     fillTrackActive: color("#8c330d"),
     strokeBg: color("#b03f0e"),
     rounding: 20,
     textSize: 20
  });
  i = createSlider("FeedBack", windowWidth - 600,windowHeight -50,100,32);
  i.setStyle(
  {
     fillBg: color("#1cbd37"),
     fillBgHover : color("#32c74b"),
     fillBgActive: color("#42c758"),
     fillTrack: color("#bd4c1a"),
     fillTrackHover: color("#c75f32"),
     fillTrackActive: color("#8c330d"),
     strokeBg: color("#b03f0e"),
     rounding: 20,
     textSize: 20
  });
  // Distortion
  g = createToggle("Distortion", windowWidth - 800,windowHeight - 150);
  g.setStyle(
  {
    fillBgOn: color("#b81212"),
    fillBgOff: color("#871616"),
    fillBgHover : color("#912d2d"),
    fillBgActive: color("#b34444"),
    fillLabel: color("#000000"),
    fillLabelHover: color("#000000"),
    fillLabelActive: color("#000000"),
    strokeBgOn: color("#a3710d"),
    rounding: 20,
    textSize: 20
  });
  h = createSlider("DriveAmount", windowWidth - 800,windowHeight - 100,100,32);
  h.setStyle(
  {
     fillBg: color("#b81212"),
     fillBgHover : color("#a13d30"),
     fillBgActive: color("#b0483a"),
     fillTrack: color("#a3710d"),
     fillTrackHover: color("#c75f32"),
     fillTrackActive: color("#8c330d"),
     strokeBg: color("#a3710d"),
     rounding: 20,
     textSize: 20
  });
  //Filter
  k = createButton("Filter", windowWidth - 1000,windowHeight - 150);
  k.setStyle({
    rounding: 20,
    fillBg: color("#653ab0"),
    fillBgHover: color("#5c3d94"),
    fillBgActive: color("#37186e"),
    strokeBg: color("#b84c16"),
  });
  // FilterJoyStick
  l = createJoystick("Panning", windowWidth - 1200, windowHeight - 150, 125,125);
  l.setStyle({
    rounding: 20,
    fillBg: color("#8f470d"),
    fillBgHover: color("#8f470d"),
    fillBgActive: color("#8f470d"),
    fillTrack: color("#aa5cd1"),
    fillTrackHover: color("#8d3cb5"),
    fillTrackActive: color("#aa5cd1"),
  });
  //Rate
  rateBtn = createSlider("Rate", windowWidth - 1600, windowHeight - 150);
  rateBtn.setStyle({
    fillBg: color("#b81212"),
    fillBgHover : color("#a13d30"),
    fillBgActive: color("#b0483a"),
    fillTrack: color("#a3710d"),
    fillTrackHover: color("#c75f32"),
    fillTrackActive: color("#8c330d"),
    strokeBg: color("#a3710d"),
    rounding: 20,
    textSize: 20
  });
  // Rec Btn
  recBtn = createCheckbox("Reverb",windowWidth - 150 ,windowHeight - 60);
  recBtn.setStyle(
  {
    fillBg: color("#ff0000"),
     fillBgHover : color("#e01414"),
     fillBgActive: color("#ffe600"),
     fillCheck: color("#000000"),
     fillCheckHover: color("#000000"),
     fillCheckActive: color("#ff7700"),
     strokeBg: color("#f6ff00"),
     rounding: 20
  });
}
// Instances
function InstancingEffects()
{
  reverb = new p5.Reverb();
  delay = new p5.Delay();
  delay.setType('pingPong');
  filter = new p5.LowPass();
  reverb.connect(filter);
  distortion = new p5.Distortion(amount, over);
  rec = new p5.SoundRecorder();
  rec.setInput();
  fft = new p5.FFT();
}
//Verb Funcs
function Verb()
{
  if (playing == true && rverbPlaying == true)
  {
    if (e.isChanged)
    {
      x = e.val;
      x = map(0,1,0,10);
      reverb.set([x],[y],[z]);
      
    }
    if (f.isChanged)
    {
      y = f.val;
      y = map (0,1,0,100);
      reverb.set([x],[y],[z]);
    }
    if (d.isPressed)
    {
      z = d.val;
      reverb.set([x],[y],[z]);
    }
  }
  reverb.connect();
  reverb.process(sound);
  rverbPlaying = true; 
}
function DiscVerb()
{
  reverb.disconnect();
  rverbPlaying = false;
}
// Dist Funcs
function Dist()
{
  if (playing == true && distPlaying == true)
  {    
    if (h.isChanged)
    {
      // let mount;
      amount = h.val;
    }
  }
  distortion.connect();
  distortion.process(sound, [amount = h.val], [over = '2x']);
  distPlaying = true;
}
function QuitDistortion()
{
  distortion.disconnect();
  distPlaying = false;
}
// Delay Funcs
function HandleDealy(dt, feedback)
{
  delay.connect();
  delay.process(sound, dt, feedback, 2000);
  delayPlaying = true;
}
function QuitDelay()
{
  delay.disconnect();
  delayPlaying = false;
}
//Filter Funcs
function HIghPass()
{ 

  velX = 0;
  VelY = 0;
  if (l.isChanged)
  {
  }
  velX += l.val.x;
  VelY += l.val.y;

  let freq = map(l.val.x, -1, 1, 20, 2000);
  let res = map(l.val.y, -1,1, 0,50);
  freq = constrain(freq, 0, 22050);
  filter.connect();
  filter.freq(freq);
  filter.res(res);    
  
  activateFilter = true;
}
function QuiutGilter()
{
  filter.disconnect();
  activateFilter = false;
}
function Texts()
{
  textSize(12);
  noStroke();
  fill(0);
  text('Sample Rate',windowWidth - 1600, windowHeight - 100 );
  noStroke();
  fill(0);
  text('Play Music and Change Parameters, to see the upcoming Result. Finnaly you can rec your Compose and store with the Rec Button ',windowWidth - 1600, windowHeight - 80, 200,200 );
  noStroke();
  fill(0);
  text('X, Y Filter Values',windowWidth - 1185, windowHeight - 155);
  noStroke();
  fill(0);
  text('Drive Amount',windowWidth - 790, windowHeight - 50);
  noStroke();
  fill(0);
  text ('Delay Time',windowWidth - 590, windowHeight - 55);
  noStroke();
  fill(0);
  text ('Feedback',windowWidth - 590, windowHeight - 5);
  noStroke();
  fill(0);
  text ('Size',windowWidth - 390, windowHeight - 55);
  noStroke();
  fill(0);
  text ('Reverse',windowWidth - 280, windowHeight - 55);
  noStroke();
  fill(0);
  text('Decay',windowWidth - 390, windowHeight - 5);
  textSize(16)
  stroke(4);
  strokeWeight(4);
  fill('red');
  text('REC',windowWidth - 150 ,windowHeight - 7);
} 
function FFTANALYZER()
{
  let waveform = fft.waveform();
  noFill();
  beginShape();
  stroke(224, 89, 16);
  strokeWeight(5);
  for (let i = 0; i < waveform.length; i++)
  {
    let x = map(i, 0, waveform.length, 0, windowWidth);
    let y = map( waveform[i], -1, 1, 0, windowHeight);
    vertex(x,y);
  }
  endShape();
}
// TouchScreen
function touchMoved() {
  // do some stuff
  return false;
}
