//precision highp float;   


int test(){
	int r = 0;
	int c;
	
	int sum = 0;
	
	int k = 0;
	while(k < 10){
		if(++k == 5)
			continue;
		
		
		
		for(r = 0; r < 10; r++)
		   for(c = 0; c < 10; ++c)
			  if (c == 5) 
			  	continue;
			  else if(c + r < 15) 
			  	sum += c*r;
			  else
			     switch(r){
			        case 5: continue;
			        default: ++sum;
			     }
		 
		if(c == 10)
			if(r == 10)
				if(r + c == 20)
					sum += r+c;
	}
	return sum;        
}
// Expected output: 10755

/*
void main()
{
  if(test() == 10755) // Assert function returns expected value
    fragColor = vec4(0,1,0,1.0); // green
  else
    fragColor = vec4(1,0,0,1.0);  // red
}
*/
