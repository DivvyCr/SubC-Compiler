int unary() {
  float f;
  int i;
  int i2;
  bool b;
  bool b2;
  int z;
  int z2;
  bool X;

  X = true;
  i2 = 0;
  f = 0.0;
  i = 1;
  b = -!f + i;
  z2 = -!f;
  b2 = true || b;
  i2 = b2;

  return i2;
}
