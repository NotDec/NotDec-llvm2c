extern int next(void);
extern void sink_a(int);
extern void sink_b(int);

int switch_continue_only_latch(int limit) {
  while (limit > 0) {
    switch (next()) {
    case 0:
      --limit;
      sink_a(limit);
      continue;
    case 1:
      limit -= 2;
      sink_b(limit);
      continue;
    default:
      return limit;
    }
  }
  return limit;
}
