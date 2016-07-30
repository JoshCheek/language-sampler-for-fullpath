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

typedef struct invocation {
  int num_args;
  char **args;
  int num_chars;
  int num_paths;
  char **relative_paths;
  int copy_result;
  int print_help;
} Invocation;

void set_invocation_counts(struct invocation *invcn, char **strings, int num_strings) {
  for(int istring=0; istring < num_strings; ++istring, ++invcn->num_chars, ++invcn->num_paths) {
    for(int ichar=0; strings[istring][ichar]; ++ichar) {
      ++invcn->num_chars;
      if(strings[istring][ichar] == '\n')
        ++invcn->num_paths;
    }
  }
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

  // invocation info
  struct invocation invcn;
  invcn.num_chars   = 0;
  invcn.num_paths   = 0;
  invcn.copy_result = 0;
  invcn.print_help  = 0;
  invcn.num_args    = argc-1;
  invcn.args        = argv+1;
  set_invocation_counts(&invcn, argv+1, argc-1);

  // break newlines (omg, this is misery >.<)
  char *all_args       = (char*)malloc(invcn.num_chars*sizeof(char));
  invcn.relative_paths = (char**)malloc(invcn.num_paths*sizeof(char*));

  invcn.relative_paths[0] = all_args;
  for(int i_from_str=0, i_to_str=1, i_all_args=0; i_from_str < invcn.num_args; ++i_from_str) {
    char *from_str = invcn.args[i_from_str];
    for(int ichar=0; from_str[ichar]; ++ichar) {
      char c = from_str[ichar];
      if(c == '\n') {
        all_args[i_all_args] = '\0';
        ++i_all_args;
        invcn.relative_paths[i_to_str] = all_args+i_all_args;
        ++i_to_str;
      } else {
        all_args[i_all_args] = c;
        ++i_all_args;
      }
    }
    all_args[i_all_args] = '\0';
    ++i_all_args;
    invcn.relative_paths[i_to_str] = all_args+i_all_args;
    ++i_to_str;
  }

  for(int i=0; i < invcn.num_paths; ++i) {
    printf("%d: %s\n", i, invcn.relative_paths[i]);
  }

  /* invcn.print_help  = remove_string(paths, &num_paths, "-h") | remove_string(paths, &num_paths, "--help"); */
  /* invcn.copy_result = remove_string(paths, &num_paths, "-c") | remove_string(paths, &num_paths, "--copy"); */

  printf("invcn.num_chars = %d\ninvcn.num_paths = %d\n",
      invcn.num_chars,
      invcn.num_paths);

  // copy nonempty paths
  /* char **paths  = argv+1; */
  /* int num_paths = argc-1; */
  /* remove_empties(paths, &num_paths); */

  if(invcn.print_help) {
    print_help();
    goto done;
  }

  // output
  /* output(paths, num_paths, cwd); */
done:
  free(all_args);
  free(invcn.relative_paths);
  return 0;
}

/* char line[1024]; */
/* scanf("%s", line); */
/* printf("line: %s\n", line); */
/* scanf("%s", line); */
/* printf("line: %s\n", line); */
