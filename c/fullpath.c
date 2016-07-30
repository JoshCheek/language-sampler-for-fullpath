#include <stdio.h>
#include <unistd.h>

void output(char const *paths[], int num_paths, char *dir) {
  int i;
  if(num_paths == 1)
    printf("%s/%s", dir, paths[0]);
  else
    for(i=0; i<num_paths; ++i)
      printf("%s/%s\n", dir, paths[i]);
}

int main(int argc, char const *argv[]) {
  char cwd[1024] = "";
  getcwd(cwd, sizeof(cwd));
  output(argv+1, argc-1, cwd);
  return 0;
}
