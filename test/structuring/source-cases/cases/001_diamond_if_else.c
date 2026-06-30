extern void sink(int);
extern void left_path(void);
extern void right_path(void);

int diamond_if_else(int x) {
  int y;
  if (x > 10) {
    left_path();
    y = x + 3;
  } else {
    right_path();
    y = x - 5;
  }
  sink(y);
  return y;
}
