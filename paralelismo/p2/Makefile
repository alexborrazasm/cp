# Use mpicc to compile MPI programs
CC=mpicc
CFLAGS=-Wall -Wextra -g
LIBS=-lm

# Define object files for each program separately
P2_MPI_OBJS=p2_mpi_collectives.o
P2_MY_OBJS=p2_my_collectives.o
P2_MY_BETTER_OBJS=p2_my_collectives_better.o
P2_NO_MPI_OBJS=p2_no_mpi.o

# Define the final programs
PROGS=p2_mpi_collectives p2_my_collectives p2_my_collectives_better p2_no_mpi

# Default target
all: $(PROGS)

# Generic rule to compile .c into .o
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

p2_mpi_collectives: $(P2_MPI_OBJS)
	$(CC) $(CFLAGS) -o $@.out $^ $(LIBS)

p2_my_collectives: $(P2_MY_OBJS)
	$(CC) $(CFLAGS) -o $@.out $^ $(LIBS)

p2_my_collectives_better: $(P2_MY_BETTER_OBJS)
	$(CC) $(CFLAGS) -o $@.out $^ $(LIBS)
	
p2_no_mpi: $(P2_NO_MPI_OBJS)
	$(CC) $(CFLAGS) -o $@.out $^ $(LIBS)

# Clean rule to remove generated files
clean:
	rm -f *.out *.o *~