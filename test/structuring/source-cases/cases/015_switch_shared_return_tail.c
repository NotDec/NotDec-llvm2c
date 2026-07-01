extern void sink(int);

int switch_shared_return_tail(int x) {
  int y;
  switch (x) {
  case 1:
    y = 11;
    break;
  case 2:
    y = 22;
    break;
  default:
    y = -x;
    break;
  }
  sink(y);
  return y;
}
