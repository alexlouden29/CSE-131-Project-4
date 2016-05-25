//precision highp float;   



int test(){
  int sum = 0;
  
	int k = 0;
	for(k = 0; k < 10; ++k){  
	  int i;
	  for(i = 0; i < 5; ++i){
		 int j = 0;
		 while(j++ < 5){
		   
		   switch(j){
		     case 1: sum += 1; continue;
		     case 2: sum += 2; if(j == 2) break;
		     default:sum += 2; break;        
		   }
		 
		   if(j == 4)
		     break;
		 }
		 
		 if(i == 3)
		 	break;
	  }
	}
	return sum; 
}
// Expected output: 280

/*
void main()
{
  if(test() == 280) // Assert function returns expected value
    gl_FragColor = vec4(0,1,0,1.0); // green
  else
    gl_FragColor = vec4(1,0,0,1.0);  // red
}
*/
