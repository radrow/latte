#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef char* string;

void printInt(int x) {
  printf("%d\n", x);
  fflush(stdout);
}

void printString(string s) {
  printf("%s\n", s);
  fflush(stdout);
}

int readInt() {
  int x;
  scanf("%d", &x);
  return x;
}

string readString() {
  char *c = NULL;
  size_t s = 0;
  scanf(" ");
  if(getline(&c, &s, stdin) == -1) {
    perror("readString: ");
  }
  return c;
}

string stringConcat(const string a, const string b) {
  const size_t la = strlen(a);
  const size_t lb = strlen(b);
  const size_t lo = la + lb + 1;
  char* o = calloc(sizeof(char), lo);
  o = strcat(o, a);
  o = strcat(o, b);
  return o;
}

void error() {
  printf("runtime error\n");
  exit(1);
}

void __virtual_method_fail() {
  fprintf(stderr, "INTERNAL ERROR: virtual method fail\n");
  exit(-1);
}


void __unreachable() {
  fprintf(stderr, "INTERNAL ERROR: unreachable code\n");
  exit(-1);
}
