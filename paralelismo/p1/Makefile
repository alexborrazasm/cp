# Use mpicc to compile MPI programs
CC=mpicc
CFLAGS=-Wall -Wextra -g
LIBS=-lm

# Define object files for each program separately
P1_OBJS=p1.o
P1_NO_MPI_OBJS=p1_no_mpi.o

# Define the final programs
PROGS=p1 p1_no_mpi

# Default target
all: $(PROGS)

# Generic rule to compile .c into .o
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Link p1 (MPI version)
p1: $(P1_OBJS)
	$(CC) $(CFLAGS) -o $@.out $^ $(LIBS)

# Link p1_no_mpi (non-MPI version)
p1_no_mpi: $(P1_NO_MPI_OBJS)
	$(CC) $(CFLAGS) -o $@.out $^ $(LIBS)

# Clean rule to remove generated files
clean:
	rm -f *.out *.o *~