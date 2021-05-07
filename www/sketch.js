let deltaAngle = 0.04;
let initial_speed = 10;

let grasResistance = 0.1;

// state of game: set_direction, rolling, reset
let game_state = "set_direction"

// layout
let flag_distance; // between 90 and 400 px

let ball_y_start = 565;
let button_text = "";

let bx;
let by;
let sizeX = 170;
let sizeY = 50;
let overBox = false;
let locked = false;
let speed = 0;
let last_end = "";



function preload() {

}

function setup() {
  golfCanvas = createCanvas(400, 700);
  golfCanvas.parent("divCollectData");
  
  // construct ball  
  ball = new Ball(width / 2, ball_y_start, "grey");
  
  // direction arrow
  dirArrow = new createVector(0, -100);

  // button box
  bx = width / 2;
  by = height - 100;
  
  // initial flag_distance
  flag_distance = random(90, 400);
}

function draw() {
  // draw gras
  background("green");
  
  // draw environment: sand, water and deep gras
  drawEnv();
  
  // draw hole
  stroke(0);
  fill("brown");
  ellipse(width / 2, flag_distance, 20, 15);

  // set direction
  if (game_state == "set_direction") {
    // direction
    if (abs(dirArrow.angleBetween(createVector(0, -1))) > 0.7) {
      deltaAngle = deltaAngle * -1;
    }
    // rotate arrow by deltaAngle radians
    dirArrow.rotate(deltaAngle)
    // moving arrow
    drawArrow(createVector(width / 2, height - 135), dirArrow, 'black');
    speed = initial_speed;
  }
  
  // ball is rolling after direction is set with fixed initial speed
  else if (game_state == "rolling") {
    if (ball.inView() && !ball.inHole(flag_distance, 15) && speed > 0) {
      ball.move(direction, speed);
      speed -= grasResistance;
    } else {
      last_end = ball.inHole(flag_distance, 15);
      
      
      //SEND DATA TO SHINY
      // dist: flag_distance - ball_y_start [px]
      // angle: direction [radians]
      // hit: last_end
      shinyDat = [ball_y_start - flag_distance,
                   direction,
                   last_end]

      Shiny.setInputValue("jsGolfData", shinyDat);
      console.log(shinyDat);
      
      
      //end
      
      game_state = "reset";
    }
  }

  ball.display();
  drawFlag(flag_distance);
  checkAndDrawButton();
}


class Ball {
  constructor(x, y, color) {
    this.x = x;
    this.y = y;
    this.color = color;
  }

  reset() {
    this.x = width / 2;
    this.y = ball_y_start;

  }

  move(direction, speed) {
    if (speed > 0) {
      this.y -= speed;
      this.x += speed * tan(-direction);
    }
  }


  inHole(hole_y, radius = 15) {
    if (dist(this.x, this.y, width / 2, hole_y) < radius) {
      //console.log("goal");
      return (true);

    } else {
      return (false);
    }


  }

  inView() {
    if (this.y > 0 &&
      this.y < height &&
      this.x > 0 &&
      this.x < width) {
      return (true)
    } else {
      return (false)
    }

  }

  display() {
    fill(this.color);
    circle(this.x, this.y, 15);
  }
}

function drawArrow(base, vec, myColor) {
  push();
  stroke(myColor);
  strokeWeight(3);
  fill(myColor);
  translate(base.x, base.y);
  line(0, 0, vec.x, vec.y);
  rotate(vec.heading());
  let arrowSize = 7;
  translate(vec.mag() - arrowSize, 0);
  triangle(0, arrowSize / 2, 0, -arrowSize / 2, arrowSize, 0);
  pop();
}

function mousePressed() {
  if (overBox) {

    if (!locked && game_state == "set_direction") {
      direction = dirArrow.angleBetween(createVector(0, -1));
      //console.log("dir: " + direction);
      game_state = "rolling";

    } else if (!locked && game_state == "reset") {  
      
      //reset
      speed = 0;
      ball.reset();
      //console.log("restart");
      game_state = "set_direction";
      flag_distance = random(90, 400);

    }

    locked = true;
    fill(255, 255, 255);
  } else {
    locked = false;
  }
}

function mouseReleased() {
  locked = false;
}

function drawFlag(y_dist) {
  //flag
  push();
  translate(-2, 0);
  fill("white");
  rect(width / 2, y_dist, 4, -20);
  fill("red");
  rect(width / 2, y_dist - 20, 4, -20);
  fill("white");
  rect(width / 2, y_dist - 40, 4, -20);
  fill("red");
  rect(width / 2, y_dist - 60, 4, -20);

  rect(width / 2 + 3, y_dist - 60, 30, -20);
  fill("white");
  circle(width / 2 + 2, y_dist - 84, 6)
  pop();
}

function checkAndDrawButton() {
  // cursor over box?
  if (
    mouseX > bx - sizeX / 2 &&
    mouseX < bx + sizeX / 2 &&
    mouseY > by - sizeY / 8 &&
    mouseY < by + sizeY
  ) {
    overBox = true;
    if (!locked) {
      stroke(200);
      fill(140);
    }
  } else {
    stroke("darkgrey");
    fill("grey");
    overBox = false;
  }

  // draw button
  push();
  translate(-sizeX / 2, 0)
  rect(bx, by, sizeX, sizeY, 20);
  strokeWeight(0.2);
  stroke(0);
  fill(0);
  textSize(20);
  textAlign(CENTER);
  
  if(game_state == "rolling") 
    button_text = "";
  else if (game_state == "set_direction")
    button_text = "Set direction";
  else if (game_state == "reset")
    if(last_end)
      button_text = "Super! Try again!"; 
    else
      button_text = "Try again!";
  
  text(button_text, bx + sizeX / 2, by + sizeY / 1.6);
  pop();

}

function drawEnv(){
  //sand 1
  push();
  fill("#d2b773");
  strokeWeight(2);
  stroke("#c6a664");
  translate(0, 0);
  beginShape();
  curveVertex(0, 0);     curveVertex(0, 0);   curveVertex(0, 150); curveVertex(50, 150);
  curveVertex(110, 100); curveVertex(100, 0); curveVertex(0, 0);   curveVertex(0, 0);

  endShape();
  pop();

  //sand 2
  push();
  fill("#d2b773");
  strokeWeight(2);
  stroke("#c6a664");
  translate(width + 30, height / 2);
  rotate(45);
  beginShape();
  curveVertex(0, 0);     curveVertex(0, 0);   curveVertex(0, 150); curveVertex(50, 150);
  curveVertex(110, 100); curveVertex(100, 0); curveVertex(0, 0);   curveVertex(0, 0);
  endShape();
  pop();

  //water
  push();
  fill("blue");
  strokeWeight(2);
  stroke("darkgreen");
  translate(0, height - 190);
  rotate(PI / 8); //radians!!!
  beginShape();
  curveVertex(-10, -10); curveVertex(-10, -10); curveVertex(-100, 150);  curveVertex(200, 150);
  curveVertex(110, 50);  curveVertex(110, -10); curveVertex(-100, -100); curveVertex(-100, -100);
  endShape();
  pop();

  //deep gras
  push();
  fill("#136d15");
  strokeWeight(2);
  stroke("darkgreen");
  translate(width, 0);
  beginShape();
  curveVertex(-10, -10); curveVertex(-10, -10); curveVertex(-100, 150);  curveVertex(200, 150);
  curveVertex(110, 50);  curveVertex(110, -10); curveVertex(-100, -100); curveVertex(-100, -100);
  endShape();
  pop();
}
