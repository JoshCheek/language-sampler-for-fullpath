#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

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
    if(result == -1) {
      // free(line); <-- check docs when I get internet, do I need to free it in this situation?
      break;
    } else if (0 < result && line[result-1] == '\n') {
      line[result-1] = '\0';
    }
    struct string_list* node = (struct string_list*)malloc(sizeof(struct string_list));
    node->string = line;
    node->next   = NULL;
    ++(invocation->num_paths);
    if(!head) head       = node;
    if(tail)  tail->next = node;
    tail = node;
  }

  invocation->relative_paths = (char**)malloc(invocation->num_paths * sizeof(char*));

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

  if(!invcn.num_paths)
    read_paths(stdin, &invcn);

  remove_empties(invcn.relative_paths, &invcn.num_paths);

  pid_t child_pid = 0;
  int filedescriptors[2];
  if(invcn.copy_result) {
    pipe(filedescriptors);
    if(!(child_pid=fork())) {
      dup2(filedescriptors[0], STDIN_FILENO);
      close(filedescriptors[0]);
      close(filedescriptors[1]);
      char* pbcopy_argv[] = {"pbcopy", 0};
      execvp(pbcopy_argv[0], pbcopy_argv);
    }
  }

  FILE *streams[3] = {stdout, NULL, NULL};
  FILE *write_stream = NULL;
  if(invcn.copy_result)
    streams[1] = write_stream = fdopen(filedescriptors[1], "w");

  if(invcn.num_paths == 1)
    for(FILE** streams_ptr=streams; *streams_ptr; streams_ptr++)
      fprintf(*streams_ptr, "%s/%s", invcn.cwd, invcn.relative_paths[0]);
  else
    for(FILE** streams_ptr=streams; *streams_ptr; streams_ptr++)
      for(int i=0; i < invcn.num_paths; ++i)
        fprintf(*streams_ptr, "%s/%s\n", invcn.cwd, invcn.relative_paths[i]);

done:
  free(all_args);
  free(invcn.relative_paths);
  if(child_pid) {
    fclose(write_stream);
    close(filedescriptors[0]);
    close(filedescriptors[1]);
    int statloc = -1;
    waitpid(child_pid, &statloc, 0);
  }
  return 0;
}
