#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <linux/kd.h>
#include <string.h>
#include <errno.h>

int main( int argc, char **argv){
  int fd,   nf=400,   nt=500,   ni;
  unsigned long int na;
  char dev[16]="/dev/console";
  if(argc >=2)nf = atoi(argv[1]);
  if(argc >=3)nt = atoi(argv[2]);
  na = (nt << 16) + 1193180 / nf;
  fd = open(dev, O_RDWR);
  ni = ioctl(fd, KDMKTONE, na);
  if(errno!=0)fprintf(stdout,"%s%5d%5d%9lx%9ld%3d%5d %s\n",
                      dev,nf,nt,na,na,ni,errno,strerror(errno));
  close(fd);return 0;
}
