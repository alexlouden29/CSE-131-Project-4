//precision highp float;   

vec2 v;		// = vec2(1.0,  2.0);
float y;	// =  3.0;

float test(){
  vec2 post = v++;  // post = (1,2)  v = (2,3)
  vec2 pre  = ++v;  // pre = (3,4), v = (3,4)
  
  float fpost = y++;  // fpost = 3, y = 4
  float fpre  = ++y;  // fpre = 5, y = 5
  
  //                3              4             3                 2            3           5     5
  return 1000000.0*v.x + 100000.0*v.y + 10000.0*pre.x + 1000.0*post.y + 100.0*fpost + 10.0*fpre + y;
}
// Expected output: 3432355

/*
void main()
{
  if(test() == 14911715) // Assert function returns expected value
    gl_FragColor = vec4(0,1,0,1.0); // green
  else
    gl_FragColor = vec4(1,0,0,1.0);  // red
}
*/
