/*
#include<signal.h>
#include<dirent.h>
#include<errno.h>
#include<sys/types.h>
#include<sys/socket.h>
#include<sys/wait.h>
#include<sys/stat.h>
#include<netinet/in.h>
#include<arpa/inet.h>
*/
#define MAX 1000
#define BUF_LENGTH 2048

// Function prototypes


LONG WriteSkt(ULONG fd, CHAR *ptr, ULONG n);
LONG ReadSkt(ULONG fd, CHAR *ptr, ULONG n);

