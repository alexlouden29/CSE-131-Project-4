int test(){
	int x[5];
	int i = 0;
	for(i = 0; i < 5; ++i){
		x[i] = 4-i;
	}
	

	for(i = 0; i < 5; i += 2){
		x[x[x[i]]] = x[x[i]];
	}

        // x = 03210
     
   for(i = 0; i < 4; i = i){
     x[++i] += 1;
     ++i;
   }     
 

	return  x[0]*10000 + x[1]*1000 + x[2]*100 + x[3]*10 + x[4]; // 4220
}

// Expected output: 4220



/*
#include <stdio.h>
int main(){
printf("%d\n", test());
}
*/
