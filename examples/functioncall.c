#include <stdio.h>

unsigned int foo(int x) {
    return x+2;
}

int main() {
  unsigned int result=0;
  unsigned int i;
  for (i=0; i<10;i++){
    result = foo(result);
  }
  return 0;
}
