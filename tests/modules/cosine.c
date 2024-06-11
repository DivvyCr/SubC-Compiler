float cosine(float x) {
  float cos;
  cos = 1.0;
  
  {
	float n; 
	float eps;
	float term; 
	float alt;

	n = 1.0;
	eps = 0.000001;
	term = 1.0;
	alt = -1.0;

	while (term > eps) { 
	  term = term * x * x / n / (n+1);
	  cos = cos + alt * term;
	  alt = -alt;
	  n = n + 2;
	}
  }

  return cos;
}
