
let deltaAngle = 0.03;
let deltaSpeed = 5;
let speed = 0;
let grasResistance = 0.4;

    // state of game: start, set_direction, set_speed, reset
let game_state = "set_direction"

// layout
let hole_y = 200;
let ball_y_start = 135;

 // speed-o-meter :)
let speedo_height = 20;
let speedo_parts = 8;
let speedo_y_position = 200;

// button
let bx;
let by;
let sizeX = 150;
let sizeY = 50;
let overBox = false;
let locked = false;

var golfCanvas;


function preload(){
  
      //grasSVG = loadImage("https://openclipart.org/download/177332/gras2.svg");
   // sandSVG = loadImage("sand.svg");

}

function setup() {
    golfCanvas = createCanvas(400, 600);
  golfCanvas.parent("divCollectData");

  //golfCanvas.style('justify-content', 'center');
 


  
// ball  
  ball = new Ball(width/2, height - ball_y_start, "grey");

  
  dirArrow = new createVector(0, -100);
  
  // button box
  bx = width/2;
  by = height -100;
  
  

}

function draw() {
  background("green");
    // hole
  stroke(0);
  fill("brown");
  ellipse(width/2, hole_y, 20, 15);
  

  
  // start line
  //fill("black");
  //rect(0, height-150, width, 5);
  
  //sand 1
  push();
  fill("#d2b773");
  strokeWeight(2);
  stroke("#c6a664");
  translate(0, 0);
beginShape();
curveVertex(0, 0);
curveVertex(0, 0);
curveVertex(0, 150);
curveVertex(50, 150);
curveVertex(110, 100);
curveVertex(100, 0);
curveVertex(0, 0);
curveVertex(0, 0);

endShape();
  pop();

    //sand 2
  push();
  fill("#d2b773");
  strokeWeight(2);
  stroke("#c6a664");
  translate(width+30, height/2);
  rotate(45);
beginShape();
curveVertex(0, 0);
curveVertex(0, 0);
curveVertex(0, 150);
curveVertex(50, 150);
curveVertex(110, 100);
curveVertex(100, 0);
curveVertex(0, 0);
curveVertex(0, 0);

endShape();
  pop();
  
  
  
  
  
  
      //water 1
  push();
  fill("blue");
  strokeWeight(2);
  stroke("darkgreen");
  translate(0, height-190);
  rotate(PI/8); //radians!!!
beginShape();
curveVertex(-10, -10);
curveVertex(-10, -10);
curveVertex(-100, 150);
curveVertex(200, 150);
curveVertex(110, 50);
curveVertex(110, -10);
curveVertex(-100, -100);
curveVertex(-100, -100);

endShape();
  pop();
  
  
  
  
  
  
      //water 1
  push();
  fill("#136d15");
  strokeWeight(2);
  stroke("darkgreen");
  translate(width, 0);
  //rotate(PI/8); //radians!!!
beginShape();
curveVertex(-10, -10);
curveVertex(-10, -10);
curveVertex(-100, 150);
curveVertex(200, 150);
curveVertex(110, 50);
curveVertex(110, -10);
curveVertex(-100, -100);
curveVertex(-100, -100);

endShape();
  pop();
  
  
  
  
  
  
  
  

  if(game_state == "set_direction"){
    // direction
   if(abs(dirArrow.angleBetween(createVector(0, -1))) > 0.7){
       deltaAngle = deltaAngle * -1;
    } 

    //console.log(dirArrow.angleBetween(createVector(0, -1)));
    dirArrow.rotate(deltaAngle)

    // moving arrow
    drawArrow(createVector(width/2, height - 135), dirArrow, 'black');
  }
  
  
  
  if(game_state == "set_speed"){

    speedOmeter();

  }
  
  else if(game_state == "rolling"){
    
    
    if(ball.inView() && !ball.inHole(hole_y, 15)){
      ball.move(direction, speed/20);
      
      speed -= grasResistance;
    } else {
      
      
      game_state = "reset";
    }
  }
  
  
  
  ball.display();
  
  
  drawFlag();
  
 // push();
 // rotate(PI/6);
 // image(sandSVG, 100, -40, 100, 200);
 // pop();
  checkAndDrawButton()
  
  
  if(frameCount % 25 == 0){
  //console.log(getColor(mouseX, mouseY));
  }
}


class Ball {
  constructor(x, y, color) {
    this.x = x;
    this.y = y;
    this.color = color;
  }
  
  reset(){
    this.x = width/2;
    this.y = height - ball_y_start;
    
  }

  move(direction, speed) {
    this.y -= speed;
    this.x += speed * tan(-direction);
  }
  
  
  inHole(hole_y, radius = 15){
    if(this.y < hole_y+radius/2 && this.y > hole_y-radius &&
       this.x > width/2 - radius && this.x < width/2 + radius){
      console.log("goal");
      return(true);
      
       } else {
         return(false);
       }
    
    
  }
  
  inView(){
    if(this.y > 0 &&
       this.y < height &&
       this.x > 0 &&
       this.x < width){
      return(true)
    } else{
      return(false)
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
    
    if(!locked && game_state == "set_direction"){
      direction = dirArrow.angleBetween(createVector(0, -1));
      console.log("dir: " + direction);
      game_state = "set_speed";
      
    }else if(!locked && game_state == "set_speed"){
      
      console.log("dir: " + speed);
      game_state = "rolling";
      
    }else if(!locked && game_state == "reset"){
      //reset
      speed = 0;
      ball.reset();
      console.log("restart");
      game_state = "set_direction";
      
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

function drawFlag(){
  //flag
  push();
  translate(-2, 0);
  fill("white");
  rect(width/2, hole_y, 4, -20);
  fill("red");
  rect(width/2, hole_y-20, 4, -20);
  fill("white");
  rect(width/2, hole_y-40, 4, -20);
  fill("red");
  rect(width/2, hole_y-60, 4, -20);
  
  rect(width/2 + 3, hole_y-60, 30, -20);
  fill("white");
  circle(width/2 + 2, hole_y - 84, 6)
   pop();
  
}

function checkAndDrawButton(){
  // cursor over box?
  if (
    mouseX > bx - sizeX/2 &&
    mouseX < bx + sizeX/2 &&
    mouseY > by - sizeY/8 &&
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
  translate(-sizeX/2, 0)
  rect(bx, by, sizeX, sizeY, 20);
  strokeWeight(.2);
  stroke(0);
  fill(0);
  textSize(20);
  textAlign(CENTER);
  text(game_state, bx+sizeX/2, by+sizeY/1.6);
  pop();
  
}

function speedOmeter(){
      push();
    translate(width/4, 0);
    fill("transparent");
    rect(0, height-speedo_y_position, width/2, speedo_height);

      strokeWeight(0);
    fill("red");
    rect(0, height-speedo_y_position, width/2/speedo_parts, speedo_height);   
      fill("orange");
    rect(1 * width/2/speedo_parts, height-speedo_y_position, width/2/speedo_parts, speedo_height); 
     fill("yellow");
    rect(2 * width/2/speedo_parts, height-speedo_y_position, width/2/speedo_parts, speedo_height); 
     fill("green");
    rect(3 * width/2/speedo_parts, height-speedo_y_position, width/2/speedo_parts, speedo_height); 

       fill("green");
    rect(4 * width/2/speedo_parts, height-speedo_y_position, width/2/speedo_parts, speedo_height);
           fill("lightgreen");
    rect((3+3/4) * width/2/speedo_parts, height-speedo_y_position, width/4/speedo_parts, speedo_height);
       fill("yellow");
    rect(5 * width/2/speedo_parts, height-speedo_y_position, width/2/speedo_parts, speedo_height); 
       fill("orange");
    rect(6 * width/2/speedo_parts, height-speedo_y_position, width/2/speedo_parts, speedo_height); 
       fill("red");
    rect(7 * width/2/speedo_parts, height-speedo_y_position, width/2/speedo_parts, speedo_height); 

      //moving speed bar
      fill("black");
      rect(speed, height-speedo_y_position, 5, 20);

      if(speed < 0 || speed > width/2){
        deltaSpeed *= -1;
      }
      speed += deltaSpeed;
    pop(); 
    
  
  
}



function getColor(x, y) {
  
  loadPixels();
  let d = pixelDensity();

  let i = 4 * d*(y * d*width + x);
  let [r, g, b] = [pixels[i], pixels[i + 1], pixels[i + 2]]; // get colors
  return(color(r, g, b).toString('#rrggbb'));
  
}





