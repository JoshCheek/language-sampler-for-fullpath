#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <stdbool.h>

typedef struct invocation {
  int num_args;
  char **args;

  char *cwd;

  int num_chars;
  int num_paths;
  bool free_paths;
  char **relative_paths;

  bool copy_result;
  bool print_help;

  pid_t child_pid;
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

void output(FILE *stream, Invocation *invocation) {
  if(invocation->num_paths == 1)
    fprintf(stream, "%s/%s", invocation->cwd, invocation->relative_paths[0]);
  else
    for(int i=0; i < invocation->num_paths; ++i)
      fprintf(stream, "%s/%s\n", invocation->cwd, invocation->relative_paths[i]);
}


void remove_empties(char **strings, int *num_strings) {
  int from, to;
  for(from=0, to=0; from<*num_strings; ++from)
    if(*strings[from])
      strings[to++] = strings[from];
  *num_strings = to;
}

void remove_string(char **strings, int*num_strings, char *to_remove, bool *removed) {
  int from=0, to=0;
  for(; from<*num_strings; ++from)
    if(0 != strcmp(strings[from], to_remove))
      strings[to++] = strings[from];
    else
      *removed = true;
  *num_strings = to;
}

void set_invocation_counts(struct invocation *invcn, char **strings, int num_strings) {
  for(int istring=0; istring < num_strings; ++istring, ++invcn->num_chars, ++invcn->num_paths)
    for(int ichar=0; strings[istring][ichar]; ++ichar) {
      ++invcn->num_chars;
      if(strings[istring][ichar] == '\n')
        ++invcn->num_paths;
    }
}

void read_paths(FILE *stream, Invocation *invocation) {
  // read lines into a list since we don't know how many there are going to be
  struct string_list {
    char* string;
    struct string_list* next;
  };

  struct string_list *head=NULL, *tail=NULL;
  invocation->num_paths = 0;
  size_t size = 0;
  char* line;
  while(1) {
    // setting line to NULL causes getline to allocate a new buffer
    // note that it's our responsibility to free this memory
    line = NULL;
    int result = getline(&line, &size, stream);
    if(result == -1)
      break;
    else if (0 < result && line[result-1] == '\n')
      line[result-1] = '\0';
    struct string_list* node = (struct string_list*)malloc(sizeof(struct string_list));
    node->string = line;
    node->next   = NULL;
    ++(invocation->num_paths);
    if(!head) head       = node;
    if(tail)  tail->next = node;
    tail = node;
  }

  invocation->relative_paths = (char**)malloc(invocation->num_paths * sizeof(char*));
  invocation->free_paths     = true;

  for(int i=0; i < invocation->num_paths; i++) {
    invocation->relative_paths[i] = head->string;
    struct string_list* next_head = head->next;
    free(head);
    head = next_head;
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
  invcn.free_paths     = false;
  invcn.relative_paths = 0; // filled in later

  invcn.copy_result    = false;
  invcn.print_help     = false;
  invcn.child_pid      = 0;
  set_invocation_counts(&invcn, argv+1, argc-1);


  // break newlines (omg, this is misery >.<)
  char *all_args       = (char*)malloc(invcn.num_chars*sizeof(char));
  invcn.relative_paths = (char**)malloc(invcn.num_paths*sizeof(char*));

  invcn.relative_paths[0] = all_args;
  for(int i_from_str=0, i_to_str=1, i_all_args=0; i_from_str < invcn.num_args; ++i_from_str) {
    char *from_str = invcn.args[i_from_str];
    for(int ichar=0; from_str[ichar]; ++ichar)
      if(from_str[ichar] == '\n') {
        all_args[i_all_args++] = '\0';
        invcn.relative_paths[i_to_str++] = all_args+i_all_args;
      } else {
        all_args[i_all_args++] = from_str[ichar];
      }
    all_args[i_all_args++] = '\0';
    invcn.relative_paths[i_to_str++] = all_args+i_all_args;
  }

  // remove empty strings and flags
  remove_empties(invcn.relative_paths, &invcn.num_paths);
  remove_string(invcn.relative_paths, &invcn.num_paths, "-h",     &invcn.print_help);
  remove_string(invcn.relative_paths, &invcn.num_paths, "--help", &invcn.print_help);
  remove_string(invcn.relative_paths, &invcn.num_paths, "-c",     &invcn.copy_result);
  remove_string(invcn.relative_paths, &invcn.num_paths, "--copy", &invcn.copy_result);

  if(invcn.print_help) {
    print_help();
    goto done;
  }

  if(!invcn.num_paths) {
    free(invcn.relative_paths);
    read_paths(stdin, &invcn);
    remove_empties(invcn.relative_paths, &invcn.num_paths);
  }

  if(invcn.copy_result) {
    int fdescs[2]; // file descriptors
    pipe(fdescs);
    if(!(invcn.child_pid=fork())) {
      dup2(fdescs[0], STDIN_FILENO);
      close(fdescs[0]);
      close(fdescs[1]);
      char* pbcopy_argv[] = {"pbcopy", 0};
      execvp(pbcopy_argv[0], pbcopy_argv);
    } else {
      FILE* write_stream = fdopen(fdescs[1], "w");
      output(write_stream, &invcn);
      fclose(write_stream);
      close(fdescs[0]);
      close(fdescs[1]);
    }
  }
  output(stdout, &invcn);

done:
  if(invcn.free_paths) {
    for(int i=0; i < invcn.num_paths; ++i)
      free(invcn.relative_paths[i]);
  }
  free(all_args);
  free(invcn.relative_paths);
  if(invcn.child_pid) {
    int statloc = -1;
    waitpid(invcn.child_pid, &statloc, 0);
  }
  return 0;
}
