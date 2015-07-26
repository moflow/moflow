int f(int y) {
  int i = 0, j = 0;
  for (i = 0; i < y; i++) {
  j++;
}

    switch(y) {
      case 0: j += 1; break;
      case 1: j += 2; break;
      case 2: j += 3; break;
      case 4: j += 4; break;
      case 5: j += 4; break;
      default: break;
    }

  return j;
}

int g(int y) {
    if (y == 42) { return 42; } else { return -1; }
}

int main(int argc, char **argv) {
    return g(42);
}
