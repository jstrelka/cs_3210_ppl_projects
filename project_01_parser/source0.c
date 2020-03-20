int main() {
  float a, result;
  int e;
  int i;
  int b [10], sum, k;
  char c;
  i = 1;
  e = 10;
  a = 2.5;
  sum = 0;
  k = 0;
  result = 1;
  while (i <= e) {
    result = result * a;
    i = i + 1;
  }
  while (k < 10) {
    b[k] = k;
    sum = sum + b[k];
    k = k + 1;
  }
  c = 'y';
}
