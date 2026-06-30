extern void sink(int);

int early_return_chain(int x, int y) {
  if (x < 0) {
    sink(-1);
    return -1;
  }
  if (x == 0) {
    sink(y);
    return y;
  }
  if (y > 10) {
    sink(y);
    return x + y;
  }
  sink(x);
  return x - y;
}
