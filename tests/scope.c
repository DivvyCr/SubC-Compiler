int scope() {
  int a;
  int b;
  a = 4;
  {
	int a;
	a = 5;
	b = a;
  }
  b = a + b;
  return b;
}
