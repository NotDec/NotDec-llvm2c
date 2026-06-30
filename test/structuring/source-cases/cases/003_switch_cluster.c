extern void sink(int);
extern void one_path(void);
extern void two_path(void);
extern void seven_path(void);
extern void default_path(void);

int switch_cluster(int x) {
  int y = 0;
  switch (x) {
  case 1:
    one_path();
    y = 11;
    break;
  case 2:
    two_path();
    y = 22;
    break;
  case 7:
    seven_path();
    y = 77;
    break;
  default:
    default_path();
    y = -1;
    break;
  }
  sink(y);
  return y;
}
