extern int next(void);
extern void sink(int);

int loop_switch_shared_latch(int limit) {
  int total = 0;
  while (limit > 0) {
    switch (next()) {
    case 0:
      total += 1;
      break;
    case 1:
      total += 2;
      break;
    case 7:
      total += 7;
      break;
    default:
      return total;
    }
    sink(total);
    --limit;
  }
  return total;
}
