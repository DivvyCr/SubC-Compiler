int fibonacci(int n) {
  int total;
  total = 0;

  {
	int first;
	int second;
	int next;
	int c;

	first = 0;
	second = 1;  
	c = 1;

	while(c < n) {
	  if (c <= 1) {
		next = c;
	  } else {
		next = first + second;
		first = second;
		second = next;
	  }    
	  c = c + 1;
	  total = total + next;
	}
  }

  return total;
}
