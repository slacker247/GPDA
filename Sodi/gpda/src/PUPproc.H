typedef struct {
  int           node;
  int           nlevels;
  int           nnodes[6];
  int           col;
  int           row;
  float         belief;
  float         disbelief;
  float         time;
  char          nodelabel[32];
  char          filename[64];
} PUPINFO;

