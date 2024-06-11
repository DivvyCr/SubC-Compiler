int a;

int global_aux() {
  a = a + 1;
  return a;
}

int global() {
  a = 5;
  return global_aux();
}
