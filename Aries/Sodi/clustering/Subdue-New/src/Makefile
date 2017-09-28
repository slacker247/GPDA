# Makefile
#
# Subdue 5
#

CC =		gcc
CFLAGS =	-Wall -O3
LDFLAGS =	-O3
# debug CFLAGS and LDFLAGS
#CFLAGS =	-g -pg -Wall -O3
#LDFLAGS =	-g -pg -O3
LDLIBS =	-lm
OBJS = 		compress.o discover.o dot.c evaluate.o extend.o graphmatch.o\
                graphops.o labels.o sgiso.o subops.o utility.o
TARGETS =	gm graph2dot mdl sgiso subdue subs2dot

all: $(TARGETS)

gm: gm_main.o $(OBJS)
	$(CC) $(LDFLAGS) -o gm gm_main.o $(OBJS) $(LDLIBS)

graph2dot: graph2dot_main.o $(OBJS)
	$(CC) $(LDFLAGS) -o graph2dot graph2dot_main.o $(OBJS) $(LDLIBS)

mdl: mdl_main.o $(OBJS)
	$(CC) $(LDFLAGS) -o mdl mdl_main.o $(OBJS) $(LDLIBS)

sgiso: sgiso_main.o $(OBJS)
	$(CC) $(LDFLAGS) -o sgiso sgiso_main.o $(OBJS) $(LDLIBS)

subdue: main.o $(OBJS)
	$(CC) $(LDFLAGS) -o subdue main.o $(OBJS) $(LDLIBS)

subs2dot: subs2dot_main.o $(OBJS)
	$(CC) $(LDFLAGS) -o subs2dot subs2dot_main.o $(OBJS) $(LDLIBS)

%.o: %.c subdue.h
	$(CC) -c $(CFLAGS) $< -o $@

install:
	cp $(TARGETS) ../bin

clean:
	/bin/rm -f *.o $(TARGETS)

