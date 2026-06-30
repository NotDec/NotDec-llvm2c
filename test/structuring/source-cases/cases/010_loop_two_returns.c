extern int next(void);

int loop_two_returns(int n) {
  int total = 0;
  while (n > 0) {
    int x = next();
    if (x < 0) {
      return total;
    }
    total += x;
    --n;
  }
  return total + 1;
}
