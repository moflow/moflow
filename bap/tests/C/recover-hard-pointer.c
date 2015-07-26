int f(int *foo) {
  int x = 10;
  switch (*foo) {
      case 0:
        x = 5;
        break;
      case 3:
        x = 6;
        break;
      case 4:
        x = 6;
      case 5:
        x = 6;
        break;
      case 7:
        x = 0;
        break;
  }
  return x;
}

int main() {
  int x = 5;
  f(&x);
}
