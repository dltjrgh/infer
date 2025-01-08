#include <stdio.h>

void null_pointer_dereference_ex01 () {
  int *p = 0;
  *p = 1;
}
