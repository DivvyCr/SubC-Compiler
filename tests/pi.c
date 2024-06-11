float pi() {
  float pi;
  pi = 3.0;

  {
	bool flag;
	int i;

	flag = true;
	i = 2;

	while(i < 100) {
	  if (flag) { pi = pi + (4.0 / (i*(i+1)*(i+2))); }
	  else      { pi = pi - (4.0 / (i*(i+1)*(i+2))); }
	  
	  flag = !flag;
	  i = i+2;
	}
  }

  return pi;
}
