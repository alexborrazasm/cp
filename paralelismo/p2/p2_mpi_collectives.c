#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>

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
        
        MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);

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

        MPI_Reduce(&count, &total_count, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

        if (!rank)
        {
            pi = ((double)total_count / (double)n) * 4.0;
            printf("pi is approx. %.16f, Error is %.16f\n", pi, fabs(pi - PI25DT));
        }
    }

    MPI_Finalize();
    return 0;
}