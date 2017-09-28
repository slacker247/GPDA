########################################################
#
# SUBDUE 4.2.d.2 Release makefile
#
########################################################


CC = gcc

CFLAGS = 
PCFLAGS = -D_PVM_SUBDUE_ -I$(PVM_ROOT)/include

LFLAGS = -lm
PLFLAGS = -lgpvm3 -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)

SRCS = abstract.c cluster.c compress.c concept.c dl.c eval.c extemp.c \
	extend.c fuzzymat.c graphop.c labels.c main.c matchq.c maths.c \
	prntstct.c pvm.c readgrph.c rvrsesub.c subdue.c subgphop.c \
	subsop.c tempop.c server.c

OBJS = $(SRCS:.c=.o)
POBJS = $(SRCS:.c=.p)

.c.o:
	$(CC) $(CFLAGS) -c $*.c

.c.p:
	$(CC) $(PCFLAGS) -c -o $*.o $*.c

Subdue: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LFLAGS)

PVMSubdue: $(POBJS)
	$(CC) $(CFLAGS) $(PCFLAGS) -o $@ $(OBJS) $(LFLAGS) $(PLFLAGS)
	cp PVMSubdue $(PVM_ROOT)/bin/$(PVM_ARCH)/PVMSubdue

clean:
	rm *.o core
