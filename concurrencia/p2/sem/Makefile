
CC=gcc
CFLAGS=-Wall -Wextra -pthread -g
LIBS=
OBJS=barber.o options.o sem.o counter.o

PROGS=barber

all: $(PROGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $<

barber: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROGS) *.o *~

