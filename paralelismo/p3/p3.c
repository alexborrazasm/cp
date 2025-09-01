/*The Mandelbrot set is a fractal that is defined as the set of points c
in the complex plane for which the sequence z_{n+1} = z_n^2 + c
with z_0 = 0 does not tend to infinity.*/

/*This code computes an image of the Mandelbrot set.*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <mpi.h>

#define DEBUG 1

#define          X_RESN  1024  /* x resolution */
#define          Y_RESN  1024  /* y resolution */

/* Boundaries of the mandelbrot set */
#define           X_MIN  -2.0
#define           X_MAX   2.0
#define           Y_MIN  -2.0
#define           Y_MAX   2.0

#define RED       "\033[1m\033[31m"
#define GREEN     "\033[1m\033[32m"
#define BLUE      "\033[1m\033[34m"
#define RST       "\033[0m"

/* More iterations -> more detailed image & higher computational cost */
#define   maxIterations  1000

typedef struct complextype 
{
    float real, imag;
} Compl;

static inline double get_seconds(struct timeval t_ini, struct timeval t_end)
{
    return (t_end.tv_usec - t_ini.tv_usec) / 1E6 +
           (t_end.tv_sec - t_ini.tv_sec);
}

int main(int argc, char *argv[])
{
    /* Mandelbrot variables */
    int i, j, k;
    Compl z, c;
    float lengthsq, temp;
    int *res = NULL;

    /* Timestamp variables */
    struct timeval ti, tf;
    double timeTotal, timeCompute, timeComm;

    /* MPI variables */
    int rank, numprocs;
    
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

    /* Gather params */
    int *recvcounts = NULL;
    int *displs = NULL;

    /* Local compute */
    int local_y = Y_RESN / numprocs;
    if (rank < Y_RESN % numprocs)
        local_y++;

    int *local_res = malloc(local_y * X_RESN * sizeof(int));

    if (!local_res) {
        fprintf(stderr, "Error allocating memory\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }
    
    recvcounts = malloc(numprocs * sizeof(int));
    displs = malloc(numprocs * sizeof(int));

    if (!recvcounts || !displs) {
        fprintf(stderr, "Error allocating memory\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }
    
    int offset = 0, rows = Y_RESN/numprocs;
    for (int i = 0; i < numprocs; i++) 
    {
        recvcounts[i] = rows;
        
        if (i < Y_RESN % numprocs)
            recvcounts[i]++;
        
        recvcounts[i] *= X_RESN; // each rows has X pixels
        
        displs[i] = offset;
        offset += recvcounts[i];     
    }
 
    /* Start measuring compute time */
    gettimeofday(&ti, NULL);

    /* Calculate and draw points */
    int start_row = displs[rank] / X_RESN; 
    for(i = 0; i < local_y; i++)
    {
        for(j = 0; j < X_RESN; j++)
        {
            z.real = z.imag = 0.0;
            c.real = X_MIN + j * (X_MAX - X_MIN)/X_RESN;
            c.imag = Y_MAX - (start_row + i) * (Y_MAX - Y_MIN) / Y_RESN;
            k = 0;

            do
            {   /* iterate for pixel color */
                temp = z.real*z.real - z.imag*z.imag + c.real;
                z.imag = 2.0*z.real*z.imag + c.imag;
                z.real = temp;
                lengthsq = z.real*z.real+z.imag*z.imag;
                k++;
            } while (lengthsq < 4.0 && k < maxIterations);

            if (k >= maxIterations) local_res[j + i*X_RESN] = 0;
            else local_res[j + i*X_RESN] = k;
        }
    }

    /* End measuring compute time */
    gettimeofday(&tf, NULL);

    timeCompute = get_seconds(ti, tf);
    fprintf(stderr, BLUE"Rank %d -> Compute time = %lf (seconds)\n"RST, 
        rank, timeCompute);

    if (!rank)  // root
    {   /* Allocate result matrix of Y_RESN x X_RESN */
        res = malloc(Y_RESN * X_RESN * sizeof(int));
        if (!res) {
            fprintf(stderr, "Error allocating memory\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
            return 1;
        }
    }
    
    /* Start measuring communication time */
    gettimeofday(&ti, NULL);

    /* Send local_res to root */
    MPI_Gatherv(local_res, local_y * X_RESN, MPI_INT, 
                res, recvcounts, displs, MPI_INT,
                0, MPI_COMM_WORLD);

    /* End measuring communication time */
    gettimeofday(&tf, NULL);

    free(recvcounts);
    free(displs);

    timeComm = get_seconds(ti, tf);
    fprintf(stderr, GREEN"Rank %d -> Communication time = %lf (seconds)\n"RST, 
        rank, timeComm);
    
    double localTime = timeCompute + timeComm;
    fprintf(stderr, "Rank %d -> Local total time %lf (seconds)\n",
         rank, localTime);

    MPI_Reduce(&localTime, &timeTotal, 1, MPI_DOUBLE, MPI_MAX, 0,
         MPI_COMM_WORLD);

    free(local_res);

    if (!rank) // root
    {   
        /* Print program total time */
        fprintf(stderr, RED"Program total time: %lf (seconds)"RST"\n", timeTotal);

        /* Print result out */
        if(DEBUG) {
            for(i =0 ; i < Y_RESN; i++) {
                for(j = 0;j < X_RESN; j++)
                    printf("%3d ", res[j + i * X_RESN]);
                printf("\n");
            }
        }
        free(res);
    }

    MPI_Finalize();
    
    return 0;
}
