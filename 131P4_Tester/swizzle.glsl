vec2 v;  // = .5,  1.5

float test(){
   vec3 t;
   t = v.xxx + v.xxx; // t = (1, 1, 1)
   vec3 f;
   f = t;  // f = (1, 1, 1)
   f.z = -1.0;   // f = (1, 1, -1)
   t.z = 0.0;    // t = (1, 1, 0)
   t.x = t.z +  2.0*t.y;      // t = (2, 1, 0)
   t.yx = t.xy + t.xy;        // t = (2, 4, 0)
   t.z -= f.z * (3.0 + t.yx).y; // t = (2, 4, 5)
   return t.x* 100.0 + t.y * 10.0 + t.z; //  245
}
