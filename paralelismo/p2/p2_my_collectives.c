#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>
#include <string.h>

void addArray(int *result, int *toAdd, int count)
{
    for (int i = 0; i < count; ++i)
        result[i] += toAdd[i];
} 

// Implements a flat-tree collective operation to perform an MPI_SUM on MPI_INT 
// data. All processes send their data to the root, which sums and stores the 
// result in recvbuf 
int MPI_FlattreeColectiva(const void *sendbuf, void *recvbuf, int count, 
    MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm)
{
    if (datatype != MPI_INT) return MPI_ERR_OP;
    if (op != MPI_SUM) return MPI_ERR_OP;

    int rank, numprocs, result;
    int *send = (int *)sendbuf, *recv = (int *)recvbuf; // Cast to int
    
    result = MPI_Comm_rank(comm, &rank);
    if (result != MPI_SUCCESS) return result;

    result = MPI_Comm_size(comm, &numprocs);
    if (result != MPI_SUCCESS) return result;

    if (rank != root) 
    {
        result = MPI_Send(sendbuf, count, datatype, root, 14869, comm);
        if (result != MPI_SUCCESS) return result;
    }
    else // root
    {
        memcpy(recv, send, count * sizeof(int)); // First add root send
        int *temp = malloc(count * sizeof(int));
        if (temp == NULL) { return MPI_ERR_OTHER; }
        for (int i = 1; i < numprocs; i++)
        {
            result = MPI_Recv(temp, count, datatype, MPI_ANY_SOURCE, 14869,
                 comm, MPI_STATUS_IGNORE);
            if (result != MPI_SUCCESS) {
                free(temp);
                return result;
            }
            addArray(recv, temp, count);
        }
        free(temp);
    }

    return MPI_SUCCESS;
}

// Broadcasts a message from the process with rank root to
// all other processes of the group
int MPI_BinomialColectiva(void *buffer, int count, MPI_Datatype datatype, 
    int root, MPI_Comm comm)
{
    if (root != 0) return MPI_ERR_ROOT;

    int rank, numprocs, result;

    result = MPI_Comm_rank(comm, &rank);
    if (result != MPI_SUCCESS) return result;

    result = MPI_Comm_size(comm, &numprocs);
    if (result != MPI_SUCCESS) return result;

    if (rank) // not root
    {
        result = MPI_Recv(buffer, count, datatype, MPI_ANY_SOURCE, 2034, comm,
             MPI_STATUS_IGNORE);
        if (result != MPI_SUCCESS) return result;
    }

    int dest;
    for (int i = 1; (dest = rank + i) < numprocs; i <<= 1) // *2
    {
        if (rank < i)
        {
            result = MPI_Send(buffer, count, datatype, dest, 2034, comm);
            if (result != MPI_SUCCESS) return result;
        }
    }

    return MPI_SUCCESS;
}

int main(int argc, char *argv[])
{
    int i, n, count, total_count = 0;
    double PI25DT = 3.141592653589793238462643;
    double pi, x, y, z;
    int rank, numprocs;
    
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

    srand(rank); 

    while (1)
    {
        if (!rank) {
            printf("Enter the number of points: (0 quits) \n");
            scanf("%d", &n);
        }
        
        MPI_BinomialColectiva(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);

        if (n == 0)
            break;

        count = 0;
        for (i = rank + 1; i <= n; i += numprocs)
        {
            x = ((double)rand()) / ((double)RAND_MAX);
            y = ((double)rand()) / ((double)RAND_MAX);
            z = sqrt((x * x) + (y * y));
            if (z <= 1.0)
                count++;
        }

        MPI_FlattreeColectiva(&count, &total_count, 1, MPI_INT, MPI_SUM, 0,
             MPI_COMM_WORLD);

        if (!rank)
        {
            pi = ((double)total_count / (double)n) * 4.0;
            printf("pi is approx. %.16f, Error is %.16f\n",
                 pi, fabs(pi - PI25DT));
        }
    }

    MPI_Finalize();
    return 0;
}