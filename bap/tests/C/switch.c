int main(int argc, char *argv[]) {

  int x = 0;

  switch(argc) {
      case 0:
        x += 0;

      case 5:
        x += 2;
        break;

      case 1:
        x += 2;

      case 2:
        x += 2;

      case 3:
        x += 2;

      default:
        x = 10;
        break;
  }

  return x;
}
