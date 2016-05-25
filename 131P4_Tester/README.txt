How to run:
place repo folder in project directory.
run ./check from within repo folder (run chmod +x check if necessary)

How to write your own tests:
Use existing tests as a template to create .glsl and .dat files.
Create .out file either by running your program or computing the result by hand.

To verify your .out file is correct:
1) copy and paste your .glsl file here: http://shdr.bkcore.com/
2) Add 'precision highp float;' as the first line in the file
3) Add the following main method at the bottom

void main(){
  if(_YourFunctionHere_() == _YourOuputHere_)
    gl_FragColor = vec4(0,1,0,1.0); // green
  else
    gl_FragColor = vec4(1,0,0,1.0);  // red
}

If your output is correct you will see a green monkey head. If your output is wrong, it'll be red.
Once you verify your test case is correct, create a pull request to share it.



Example (using swizzle.glsl):
/******Original test case *******/
vec2 v =  // = .5,  1.5
float test(){
   vec3 t;
   t = v.xxx + v.xxx; // t = (1, 1, 1)
   vec3 f;
   f = t;  // f = (1, 1, 1)
   f.z = -1.0;   // f = (1, 1, -1)
   t.z = 0.0;    // t = (1, 1, 0)
   t.x = t.z + t.y;        // t = (0, 1, -1)
   t.yx = t.xy + t.xy;     // t = (2, 0, -1)
   return t.x; // 2.0
}

/***** Input for online compiler  ******/

precision highp float;   // This is always required for shdr.bkcore.com
vec2 v = vec2(.5, 1.5);  // Add initializers that were declared in .dat file


float test(){
   vec3 t;
   t = v.xxx + v.xxx;
   vec3 f;
   f = t;  
   f.z = -1.0;  
   t.z = 0.0;  
   t.x = t.z + t.y;
   t.yx = t.xy + t.xy;   
   return t.x;
}

void main() {
  if(test() == 2.0) // Assert function call returns expected value
    gl_FragColor = vec4(0,1,0,1.0); // green
  else
    gl_FragColor = vec4(1,0,0,1.0);  // red
}
