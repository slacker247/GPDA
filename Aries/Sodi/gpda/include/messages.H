//
//   Define input update messages
//
#define M_NUDET     100
#define M_EVID      101
#define M_MINE      102
#define M_STATS     103
#define M_LIMIT     104
#define M_INTEL     105
//
//   Define input alert messages
//
#define M_ASSESS    201
//
//   Define output messages
//
#define M_ISSUE     301
//
//   Define message structures
//
typedef struct {
  char          src[8];
  char          dst[8];
  char          msn[40];
  int           msgtype;
  int           ack;
  int           bytes;
} M_Header;

struct INMSG {
  char          src[8];
  char          dst[8];
  char          msn[40];
};

struct STATSMSG {
  M_Header     header;
  int          leakers;
};
