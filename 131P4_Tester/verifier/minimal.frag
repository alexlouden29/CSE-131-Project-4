/* 
How to build:
g++ textfile.h textfile.cpp ogl3.cpp -lGLU -lGL -lglut -lGLEW

Requires freeglut and glew

You can use this to verify the accuracy of test cases by editing the test() function and 
changing the condition in main().

Output will be green if test is correct and red otherwise. 
*/

#version 300 es
precision highp float;

out vec4 fragColor;



int y =  3;


int test2(){
   return ++y;
}


int test(){
  y = 1;
  int y = 10;   // y = 10, GlobalY = 1
  
  {
        int y = ++y;  // y = 11  See pg 44 of spec
  }
  
  int i = 0;
  for(i = 0; i < 10; ++i)
       int y = 0;
  
  while(i == 10){
    ++y;
    int y = 0;
    i = 1;
  }
  // y = 12, GlobalY = 1
  
  
  if(i != 0)
     int y = 0;
     
  int globalY = test2();
  // y = 12, GlobalY = 2
  
  return globalY*1000 + y;  // 2012
}
// Expected output: 2012


void main()
{
  if(test() == 2012) // Assert function returns expected value
    fragColor = vec4(0,1,0,1.0); // green
  else
    fragColor = vec4(1.0,0,0,1.0);  // red
}

