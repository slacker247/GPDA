#include <stdio.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>

main()
{
struct stat         stat_buf;
int                 fd;

   fd = open( "filename", O_RDONLY);
   fstat (fd, &stat_buf);
}
