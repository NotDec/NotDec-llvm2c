extern int next(void);
extern void sink(int);

int switch_continue_latch(int limit) {
  int total = 0;
  while (limit > 0) {
    switch (next()) {
    case 0:
      --limit;
      continue;
    case 1:
      total += 1;
      break;
    default:
      sink(total);
      break;
    }
    --limit;
  }
  return total;
}
