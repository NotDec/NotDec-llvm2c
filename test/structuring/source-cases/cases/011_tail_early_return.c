extern int next(void);

int tail_early_return(int n) {
  int total = 0;
  while (n-- > 0) {
    int x = next();
    total += x;
    if (x < 0) {
      return total;
    }
  }
  return total + 1;
}
