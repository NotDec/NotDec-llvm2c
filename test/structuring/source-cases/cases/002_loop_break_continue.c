extern int next(void);
extern void sink(int);

int loop_break_continue(int n) {
  int sum = 0;
  while (n > 0) {
    int x = next();
    if (x < 0) {
      break;
    }
    if (x == 0) {
      --n;
      continue;
    }
    sum += x;
    --n;
  }
  sink(sum);
  return sum;
}
