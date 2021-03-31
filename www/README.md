To include sketch.js in shiny app:
- create named canvas
- define parent to be "divCollectData" 
in setup function.

'''
var golfCanvas;
function setup() {
  golfCanvas = createCanvas(400, 600);
  golfCanvas.parent("divCollectData");
  ...
}
'''
