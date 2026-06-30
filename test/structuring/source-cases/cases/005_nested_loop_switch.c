extern int next(void);
extern void sink(int);

int nested_loop_switch(int limit) {
  int total = 0;
  while (limit > 0) {
    int code = next();
    switch (code) {
    case 0:
      --limit;
      continue;
    case 1:
      total += code;
      break;
    default:
      sink(total);
      return total;
    }
    sink(total);
    --limit;
  }
  return total;
}
