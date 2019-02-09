#include <stdio.h>
#include <unistd.h>

int main (int argc, char* argv[])  {
  (void)argc;
  for ( int i = 1; i < argc; i++ )  {
    char* file = argv[i];
    int ok = access(file, X_OK);
    printf("%s %d\n", file, ok);
  }
  return 0;
}
