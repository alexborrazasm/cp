
CC=gcc
CFLAGS=-Wall -Wextra -pthread -g
LIBS=
OBJS=test.o rw_mutex.o

PROGS=test

all: $(PROGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $<

test: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROGS) *.o *~

