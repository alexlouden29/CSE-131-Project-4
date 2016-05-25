//precision highp float;   


int test(){
	int i = -1;
	int j = 2;
	float f = -1.0;
	float g = 2.0;
	
	int bitString = 0;
	
	
	if(i < j)
		bitString += 1;
	if(i <= j)
		bitString += 2;
	if(i == j)
		bitString += 4;
	if(i >= j)
		bitString += 8;
	if(i > j)
		bitString += 16;
	if(i != j)
		bitString += 32;
		
	
	if(f < g)
		bitString += 64;
	if(f <= g)
		bitString += 128;
	if(f == g)
		bitString += 256;
	if(f >= g)
		bitString += 512;
	if(f > g)
		bitString += 1024;
	if(f != g)
		bitString += 2048;
		
	
	////////////////////////////////////////////////////
	if(j < i)
		bitString += 4096;
	if(j <= i)
		bitString += 8192;
	if(j == i)
		bitString += 16384;
	if(j >= i)
		bitString += 32768;
	if(j > i)
		bitString += 65536;
	if(j != i)
		bitString += 131072;
		
	
	if(g < f)
		bitString += 262144;
	if(g <= f)
		bitString += 524288;
	if(g == f)
		bitString += 1048576;
	if(g >= f)
		bitString += 2097152;
	if(g > f)
		bitString += 4194304;
	if(g != f)
		bitString += 8388608;	
	
	return bitString;  
}
// Expected output: 14911715 (In binary: 111000111000100011100011)

/*
void main()
{
  if(test() == 14911715) // Assert function returns expected value
    gl_FragColor = vec4(0,1,0,1.0); // green
  else
    gl_FragColor = vec4(1,0,0,1.0);  // red
}
*/
