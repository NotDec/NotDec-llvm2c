extern void sink(int);

int switch_early_return(int x) {
  int y = x;
  switch (x) {
  case 0:
    return 0;
  case 1:
    y = x + 10;
    break;
  case 2:
    y = x + 20;
    break;
  default:
    y = -x;
    break;
  }
  sink(y);
  return y;
}
