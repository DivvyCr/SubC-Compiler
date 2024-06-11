int recursive_add(int n) {
  int result;
  result = 0;

  if(n != 0) {
	result = n + recursive_add(n-1);
  } else {
	result  = n;
  }

  return result;
}
