int main() {
  int a;
  int *p1;
  int *p2;
  a = 18;
  debug a;
  p1 = &a;
  p2 = &a;
  debug *p1;
  debug *p2;
  *p1=24;
  debug a;
  debug *p1;
  debug *p2;
}
