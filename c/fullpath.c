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

void remove_empties(char **strings, int *num_strings) {
  int from, to;
  for(from=0, to=0; from<*num_strings; ++from)
    if(*strings[from])
      strings[to++] = strings[from];
  *num_strings = to;
}

int remove_string(char **strings, int*num_strings, char *to_remove) {
  int from=0, to=0, removed=0;
  for(; from<*num_strings; ++from)
    if(0 != strcmp(strings[from], to_remove))
      strings[to++] = strings[from];
    else
      removed = 1;
  *num_strings = to;
  return removed;
}

void print_help() {
  printf("usage: fullpath *[relative-paths] [-c]\n");
  printf("\n");
  printf("  Prints the fullpath of the paths\n");
  printf("  If no paths are given as args, it will read them from stdin\n");
  printf("\n");
  printf("  If there is only one path, the trailing newline is omitted\n");
  printf("\n");
  printf("  The -c flag will copy the results into your pasteboard\n");
}

int main(int argc, char **argv) {
  // get current working directory
  char cwd[1024];
  getcwd(cwd, sizeof(cwd));

  // copy nonempty paths
  char **paths  = argv+1;
  int num_paths = argc-1;
  remove_empties(paths, &num_paths);
  int do_help   = remove_string(paths, &num_paths, "-h") | remove_string(paths, &num_paths, "--help");
  int do_copy   = remove_string(paths, &num_paths, "-c") | remove_string(paths, &num_paths, "--copy");

  if(do_help) {
    print_help();
    return 0;
  }

  // output
  output(paths, num_paths, cwd);
  return 0;
}

/* char line[1024]; */
/* scanf("%s", line); */
/* printf("line: %s\n", line); */
/* scanf("%s", line); */
/* printf("line: %s\n", line); */
