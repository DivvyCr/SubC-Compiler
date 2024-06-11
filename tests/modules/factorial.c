int factorial(int n) {
  int factorial;
  factorial = 1;

  {
	int i;
	i = 1;

	while (i <= n) {
	  factorial = factorial * i;
	  i = i + 1 ;
	}
  }
    
  return factorial;
}
