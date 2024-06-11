#include <cstdio>
#include <iostream>
#include <math.h>
#include <sstream>
#include <string>

extern "C" void print_int(int n) {
  fprintf(stderr, "%d\n", n);
}

extern "C" void print_float(float n) {
  fprintf(stderr, "%f\n", n);
}

extern "C" {
  int arithmetic(int a, int b, int c);
  int factorial(int n);
  int fibonacci(int n);
  int recursive_add(int n);
  int recursive_fac(int n);
  float pi();
  float cosine(float n);
  bool palindrome(int n);

  int assignment();
  int associativity();
  int cast();
  int global();
  int lazy_and(int x);
  int lazy_or(int x);
  int returns(int x);
  int scope();
  int unary();
}

bool approx_equal(float a, float b, float err) {
  return fabs(a - b) <= fmin(fabs(a), fabs(b)) * err;
}

void print_result(const std::string &test_str, bool test_result) {
  std::string PASSED = "\033[32mPASSED\033[0m";
  std::string FAILED = "\033[31mFAILED\033[0m";

  int padding = 18 - test_str.size();
  std::stringstream padstream;
  while (padding-- > 0) { padstream << " "; }
  std::cout << padstream.str() << test_str << " " << (test_result ? PASSED : FAILED) << std::endl;
}

int main() {
  std::cout << "              TEST RESULT" << std::endl;
  std::cout << "              ---- ------" << std::endl;

  print_result("Arithmetic", (arithmetic(2, 3, 4) == 14));
  print_result("Factorial", (factorial(11) == 39916800));
  print_result("Fibonacci", (fibonacci(10) == 88));
  print_result("Recursion (add)", (recursive_add(20) == 210));
  print_result("Recursion (fac)", (recursive_fac(11) == 39916800));
  print_result("Palindrome", (palindrome(111) && palindrome(79100197) && !palindrome(10) && !palindrome(84091)));

  float actual_pi = 3.1415926535f;
  print_result("Pi", (approx_equal(pi(), actual_pi, 0.000001f)));
  print_result("Cosine", (approx_equal(cosine(2*actual_pi/3), -0.5f, 0.0001f)));

  print_result("Assignments", (assignment() == 8));
  print_result("Associativity", (associativity() == -4));
  print_result("Cast", (cast() == 3));
  print_result("Globals", (global() == 6));
  print_result("Lazy (AND)", (lazy_and(1) == 1 && lazy_and(0) == 0));
  print_result("Lazy (OR)", (lazy_or(1) == 0 && lazy_or(0) == 1));
  print_result("Early Returns", (returns(1) == 0 && returns(2) == 1 && returns(-1) == 2));
  print_result("Scope", (scope() == 9));
  print_result("Unary", (unary() == 1));
}
