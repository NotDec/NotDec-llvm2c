extern int next(void);
extern void sink(int);

int nested_loop_break_continue(int outer, int inner) {
  int total = 0;
  while (outer > 0) {
    int j = inner;
    while (j > 0) {
      int x = next();
      if (x < 0) {
        break;
      }
      if (x == 0) {
        --j;
        continue;
      }
      total += x;
      --j;
    }
    sink(total);
    --outer;
  }
  return total;
}
