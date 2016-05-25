vec2 v;  // = .5,  1.5

float vectassign()
{
   vec2 t;
   t = v;
   t = t + v;   // t = 1, 3
   t = t * v;   // t = .5, 4.5
   t = v - t;   // t = 0, -3
   
   v = t + v;  // v = .5, -1.5
   
   t += t;  // 0, -6
   t *= t;  // 0, 36
   t += v;  // .5, 34.5
   t /= v;  // 1, -23
   

   return t.y - t.x;  // -23 - 1 = -24
}

