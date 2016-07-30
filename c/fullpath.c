#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

typedef struct invocation {
  int num_args;
  char **args;

  char *cwd;

  int num_chars;
  int num_paths;
  char **relative_paths;

  int copy_result;
  int print_help;
} Invocation;

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


void output(Invocation *invocation) {
  if(invocation->print_help)
    print_help();
  else if(invocation->num_paths == 1)
    printf("%s/%s", invocation->cwd, invocation->relative_paths[0]);
  else
    for(int i=0; i < invocation->num_paths; ++i)
      printf("%s/%s\n", invocation->cwd, invocation->relative_paths[i]);
}


void remove_empties(char **strings, int *num_strings) {
  int from, to;
  for(from=0, to=0; from<*num_strings; ++from)
    if(*strings[from])
      strings[to++] = strings[from];
  *num_strings = to;
}

void remove_string(char **strings, int*num_strings, char *to_remove, int *removed) {
  int from=0, to=0;
  for(; from<*num_strings; ++from)
    if(0 != strcmp(strings[from], to_remove))
      strings[to++] = strings[from];
    else
      *removed = 1;
  *num_strings = to;
}

void set_invocation_counts(struct invocation *invcn, char **strings, int num_strings) {
  for(int istring=0; istring < num_strings; ++istring, ++invcn->num_chars, ++invcn->num_paths) {
    for(int ichar=0; strings[istring][ichar]; ++ichar) {
      ++invcn->num_chars;
      if(strings[istring][ichar] == '\n')
        ++invcn->num_paths;
    }
  }
}

int main(int argc, char **argv) {
  // invocation info
  struct invocation invcn;
  invcn.num_args       = argc-1;
  invcn.args           = argv+1;

  char cwd[1024];
  invcn.cwd            = cwd;
  getcwd(invcn.cwd, sizeof(cwd));

  invcn.num_chars      = 0;
  invcn.num_paths      = 0;
  invcn.relative_paths = 0; // filled in later

  invcn.copy_result    = 0;
  invcn.print_help     = 0;
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

  // remove empty strings and flags
  remove_empties(invcn.relative_paths, &invcn.num_paths);
  remove_string(invcn.relative_paths, &invcn.num_paths, "-h",     &invcn.print_help);
  remove_string(invcn.relative_paths, &invcn.num_paths, "--help", &invcn.print_help);
  remove_string(invcn.relative_paths, &invcn.num_paths, "-c",     &invcn.copy_result);
  remove_string(invcn.relative_paths, &invcn.num_paths, "--copy", &invcn.copy_result);

  output(&invcn);
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
