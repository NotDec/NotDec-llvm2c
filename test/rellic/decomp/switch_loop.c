int printf(const char*, ...);

int main() {
  int i = 0;
  start:
  i++;
  switch(i) {
    case 1: printf("%d%d\n", 1, i); goto start; break;
    case 2: printf("%d%d\n", 2, i); goto start; break;
    case 3: printf("%d%d\n", 3, i); break;
  }
}
