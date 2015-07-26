#include <stdio.h>

int f(int input) {
  int x = 0;
  while (x < 100) {
    switch (x) {
        case 0: x += input; break;
        case 1: x += 1; break;
        case 2: x += 6; break;
        case 3: x += 5; break;
        case 4: x += 4; break;
        case 5: x += 3; break;
        case 6: x += 2; break;
    }
  }

  printf("woo\n");
  return x;
}

int main() {
  f(3);
}
