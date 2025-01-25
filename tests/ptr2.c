int main() {
  int a1;
  int a2;
  int *p1;
  int *p2;
  int **p11;
  int **p12;
  int **p22;
  /* Assign values */
  a1 = 1;
  a2 = 2;
  p1 = &a1;
  p2 = &a2;
  p11 = &p1;
  p12 = &p1;
  p22 = &p2;
  /* print tests */
  debug a1;
  debug a2;
  debug *p1;
  debug *p2;
  debug **p11;
  debug **p12;
  debug **p22;
  /* change tests values */
  p12=&p2;
  debug **p12;
  *p12=p1;
  debug *p1;
  debug *p2;

}
