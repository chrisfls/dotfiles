#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
  char *args = (char *) malloc(sizeof(char));

  args[0] = '\0';

  for (int i = 1; i < argc; i++) {
    char *path = (char *) malloc(strlen(argv[i]) * 3 + 1);
    sprintf(path, "$(wslpath \\\"%s\\\")", argv[i]);
    args = (char *) realloc(args, strlen(args) + strlen(path) + 1);
    strcat(args, path);
    strcat(args, " ");
    free(path);
  }

  char *command = (char *) malloc(strlen(args) + strlen(EXEC_NAME) + 50);
  sprintf(command, "bash --login -c \"direnv exec . %s %s\"", EXEC_NAME, args);

  system(command);

  free(args);
  free(command);

  return 0;
}
