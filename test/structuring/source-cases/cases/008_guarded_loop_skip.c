extern int next(void);
extern void sink(int);

int guarded_loop_skip(int n) {
  int total = 0;
  while (n > 0) {
    total += next();
    --n;
  }
  sink(total);
  return total;
}
