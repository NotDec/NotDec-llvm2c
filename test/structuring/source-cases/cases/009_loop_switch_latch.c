extern int next(void);

int loop_switch_latch(int limit) {
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
      return total;
    }
    --limit;
  }
  return total;
}
