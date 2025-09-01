#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include "rw_mutex.h"

#define READERS    10
#define WRITERS    20
#define ITERATIONS_W 10
#define ITERATIONS_R 20

#define RST    "\033[0m"
#define RED    "\033[1m\033[31m"
#define GREEN  "\033[1m\033[32m"
#define YELLOW "\033[1m\033[33m"
#define BLUE   "\033[1m\033[34m"

struct thread_info {
    pthread_t thread_id;   // id returned by pthread_create()
    int       thread_num;  // application defined thread #
};

struct args {
    int thread_num;  // application defined thread #
    int *n;          // shared int
    int it;
    rw_mutex_t *m;
};

void *writer(void *ptr)
{
    struct args *args = ptr;
    
    while (args->it--)
    {
        rw_mutex_writelock(args->m);       
        (*args->n)++;
        printf(BLUE"Writer %4d: %6d\n"RST, args->thread_num, *args->n);
        rw_mutex_writeunlock(args->m);

        usleep(50);
    }

    return NULL;
}

void *reader(void *ptr)
{
    struct args *args = ptr;

    while (args->it--)
    {
        rw_mutex_readlock(args->m);       
        printf(GREEN"Reader %4d: %6d\n"RST, args->thread_num, *args->n);
        rw_mutex_readunlock(args->m);
    
        usleep(50);
    }
    return NULL;
}
void startThr()
{
    int i;
    struct thread_info *infoR, *infoW;
    struct args *argsR, *argsW;
    int *number;
    rw_mutex_t *m;

    infoR  = malloc(sizeof(struct thread_info) * READERS);
    infoW  = malloc(sizeof(struct thread_info) * WRITERS);
    argsR  = malloc(sizeof(struct args) * READERS);
    argsW  = malloc(sizeof(struct args) * WRITERS);
    m      = malloc(sizeof(rw_mutex_t));
    number = malloc(sizeof(int));

    if (!infoR || !infoW || !argsR || !argsW || !m || !number)
    {
        printf("Out of memory\n");
        exit(1); 
    }
    
    // Start rw_mutex
    if (rw_mutex_init(m)) {
        printf("Can not initiate mutex\n");
        exit(1);
    }

    *number = 0;

    for (i = 0; i < READERS; i++)
    {
        infoR[i].thread_num = i;
        argsR[i].thread_num = i;
        argsR[i].m = m;
        argsR[i].n = number;
        argsR[i].it = ITERATIONS_R;

        if ( 0 != pthread_create(&infoR[i].thread_id, NULL, reader,
             &argsR[i]))
        {
            printf("Could not create thread #%d\n", i);
            exit(1);
        }
    }

    for (i = 0; i < WRITERS; i++)
    {
        infoW[i].thread_num = i;
        argsW[i].thread_num = i;
        argsW[i].m = m;
        argsW[i].n = number;
        argsW[i].it = ITERATIONS_W;

        if ( 0 != pthread_create(&infoW[i].thread_id, NULL, writer,
             &argsW[i]))
        {
            printf("Could not create thread #%d\n", i);
            exit(1);
        }
    }

    // wait for the threads to finish
    for (i = 0; i < READERS; i++)
        pthread_join(infoR[i].thread_id, NULL);

    for (i = 0; i < WRITERS; i++)
        pthread_join(infoW[i].thread_id, NULL);

    // Destroy rw_mutex
    rw_mutex_destroy(m);
    
    // Free allocated memory
    free(infoR);
    free(infoW);
    free(argsR);
    free(argsW);
    free(m);
    free(number);
}

int main()
{
    startThr();
    return 0;
}