
CC=gcc
CFLAGS=-Wall -Wextra -pthread -g
LIBS=
OBJS=swap.o options.o op_count.o it_count.o


PROGS= swap

all: $(PROGS)

%.o : %.c
	$(CC) $(CFLAGS) -c $<

swap: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROGS) *.o *~

