#include <stdio.h>

void print_paths(char const *paths[], int num_paths) {
  int i;
  if(num_paths == 1)
    printf("%s", paths[0]);
  else
    for(i=0; i<num_paths; ++i)
      printf("%s\n", paths[i]);
}

int main(int argc, char const *argv[]) {
  print_paths(argv+1, argc-1);
  return 0;
}
