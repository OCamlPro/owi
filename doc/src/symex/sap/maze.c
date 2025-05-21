#include <owi.h>

// example from https://feliam.wordpress.com/2010/10/07/the-symbolic-maze/

#define H 7
#define W 11
#define ITERS 28

char maze[H][W] = {
  "+-+---+---+",
  "| |     |#|",
  "| | --+ | |",
  "| |   | | |",
  "| +-- | | |",
  "|     |   |",
  "+-----+---+"
};

int main (void) {

  int x = 1;
  int y = 1;
  maze[y][x]='X';

  char program[ITERS];

  for (int i = 0; i < ITERS; i++) {
    program[i] = owi_char();
  }

  int old_x = x;
  int old_y = y;

  for (int i = 0; i < ITERS; i++) {

    old_x = x;
    old_y = y;

    switch (program[i]) {
      case 'w':
        y--;
        break;
      case 's':
        y++;
        break;
      case 'a':
        x--;
        break;
      case 'd':
        x++;
        break;
      default:
        return 1;
    }

    if (maze[y][x] == '#') {
      // TODO: print the result
      owi_assert(0);
      return 0;
    }

    if (maze[y][x] != ' ' && !((y == 2 && maze[y][x] == '|' && x > 0 && x < W))) {
      return 1;
    }

    if (old_x == x && old_y == y) {
      return 1;
    }

    maze[y][x] = 'X';
  }
  return 1;
}
