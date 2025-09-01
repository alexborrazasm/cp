# Use mpicc to compile MPI programs
CC=mpicc
CFLAGS=-o2 -march=native -Wall -Wextra -g
LIBS=-lm

# Define object files for each program separately
P3_OBJS=p3.o
MANDEL_OBJS=mandel.o

# Define the final programs
PROGS=p3 mandel 

# Default target
all: $(PROGS)

# Generic rule to compile .c into .o
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

p3: $(P3_OBJS)
	$(CC) $(CFLAGS) -o $@.out $^ $(LIBS)

mandel: $(MANDEL_OBJS)
	$(CC) $(CFLAGS) -o $@.out $^ $(LIBS)

# Clean rule to remove generated files
clean:
	rm -f *.out *.o *~