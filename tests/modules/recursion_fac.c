int recursive_fac(int n) {
  int result;
  result = 0;

  if (n >= 1) {
	result = n * recursive_fac(n-1);
  } else {
	result = 1;
  }

  return result;
}
