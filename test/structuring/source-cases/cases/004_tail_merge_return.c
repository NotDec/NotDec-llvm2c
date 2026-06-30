extern void sink(int);

int tail_merge_return(int x, int y) {
  int result;
  if (x < 0) {
    sink(-1);
    result = -1;
  } else if (y < 0) {
    sink(-2);
    result = -2;
  } else {
    sink(1);
    result = x + y;
  }
  sink(result);
  return result;
}
