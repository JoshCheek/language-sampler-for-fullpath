#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

void output(char *paths[], int num_paths, char *dir) {
  if(num_paths == 1)
    printf("%s/%s", dir, paths[0]);
  else
    for(int i=0; i<num_paths; ++i)
      printf("%s/%s\n", dir, paths[i]);
}

void remove_empties(char **from_strings, int num_from_strings, char **to_strings, int *num_to_strings) {
  *num_to_strings = 0;
  for(int i=0; i<num_from_strings; ++i)
    if(*from_strings[i])
      to_strings[(*num_to_strings)++] = from_strings[i];
}

int main(int argc, char **argv) {
  // get current working directory
  char cwd[1024];
  getcwd(cwd, sizeof(cwd));

  // copy nonempty paths
  char **paths  = (char**)malloc(argc*sizeof(char*));
  int num_paths = 0;
  remove_empties(argv+1, argc-1, paths, &num_paths);

  // output
  output(paths, num_paths, cwd);
  return 0;
}
