//
//   Define input update messages
//
#define M_NUDET     1
#define M_EVID      2
#define M_LIMIT     3
#define M_STATS     4
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
  int           bytes;
  int           ack;
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
