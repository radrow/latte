#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef char* string;

void printInt(int x) {
  printf("%d\n", x);
}

void printString(string s) {
  printf(s);
}

int readInt() {
  int x;
  scanf("%d", &x);
  return x;
}

string readString() {
  char *c = NULL;
  size_t s = 0;
  if(getline(&c, &s, stdin) == -1) {
    perror("readString: ");
  }
  return c;
}

string stringConcat(const string a, const string b) {
  size_t la = strlen(a);
  size_t lb = strlen(b);
  size_t lo = la + lb;
  char* o = calloc(sizeof(char), lo);
  o = strcat(o, a);
  o = strcat(o, b);
  return o;
}

void error() {
  printf("runtime error\n");
  exit(1);
}
