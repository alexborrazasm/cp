#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "op_count.h"
#include "options.h"

struct buffer {
    int *data;
    int size;
};

struct thread_info {
    pthread_t       thread_id;        // id returned by pthread_create()
    int             thread_num;       // application defined thread #
};

struct args {
    int				thread_num;       // application defined thread #
    int				delay;			  // delay between operations
    int				iterations;
    struct buffer	*buffer;		  // Shared buffer
    pthread_mutex_t *mutexes;         // mutexes array
};

void lock_positions(pthread_mutex_t *mutexes, int pos1, int pos2)
{
    int first, second;

    first = (pos1 < pos2) ? pos1 : pos2;
    second = (pos1 < pos2) ? pos2 : pos1;
    
    pthread_mutex_lock(&mutexes[first]);
    if (first != second) {
        pthread_mutex_lock(&mutexes[second]);
    }
}

void unlock_positions(pthread_mutex_t *mutexes, int pos1, int pos2)
{
    int first, second;
    
    first = (pos1 < pos2) ? pos1 : pos2;
    second = (pos1 < pos2) ? pos2 : pos1;
    
    if (first != second) {
        pthread_mutex_unlock(&mutexes[second]);
    }
    pthread_mutex_unlock(&mutexes[first]);
}

void *swap(void *ptr)
{
    struct args *args =  ptr;

    while(args->iterations--) {
        int i, j, tmp;
        i = rand() % args->buffer->size;
        j = rand() % args->buffer->size;

        lock_positions(args->mutexes, i, j);

        printf("Thread %d swapping positions %d (== %d) and %d (== %d)\n",
            args->thread_num, i, args->buffer->data[i], j, args->buffer->data[j]);

        tmp = args->buffer->data[i];
        if(args->delay) usleep(args->delay); // Force a context switch

        args->buffer->data[i] = args->buffer->data[j];
        if(args->delay) usleep(args->delay);

        args->buffer->data[j] = tmp;
        if(args->delay) usleep(args->delay);

        unlock_positions(args->mutexes, i, j);

        inc_count();

    }
    return NULL;
}

int cmp(int *e1, int *e2) {
    if(*e1==*e2) return 0;
    if(*e1<*e2) return -1;
    return 1;
}

void print_buffer(struct buffer buffer) {
    int i;

    for (i = 0; i < buffer.size; i++)
        printf("%i ", buffer.data[i]);
    printf("\n");
}

void start_threads(struct options opt)
{
    int i;
    struct thread_info *threads;
    struct args *args;
    struct buffer buffer;
    pthread_mutex_t *mutexes;

    srand(time(NULL));
    
    buffer.data = malloc(opt.buffer_size * sizeof(int));
    mutexes = malloc(sizeof(pthread_mutex_t) * opt.buffer_size);

    if(buffer.data == NULL || mutexes == NULL ) {
        printf("Out of memory\n");
        exit(1);
    }
    buffer.size = opt.buffer_size;

    for(i=0; i<buffer.size; i++) {
        buffer.data[i]=i;
        if (pthread_mutex_init(&mutexes[i], NULL))
        {
            printf("Can not initiate mutex\n");
            exit(1);
        }
    }

    printf("creating %d threads\n", opt.num_threads);
    threads = malloc(sizeof(struct thread_info) * opt.num_threads);
    args = malloc(sizeof(struct args) * opt.num_threads);

    if (threads == NULL || args==NULL || mutexes == NULL) {
        printf("Not enough memory\n");
        exit(1);
    }

    printf("Buffer before: ");
    print_buffer(buffer);

    // Create num_thread threads running swap()
    for (i = 0; i < opt.num_threads; i++) {
        threads[i].thread_num = i;

        args[i].thread_num = i;
        args[i].buffer     = &buffer;
        args[i].delay      = opt.delay;
        args[i].iterations = opt.iterations;
        args[i].mutexes      = mutexes;

        if ( 0 != pthread_create(&threads[i].thread_id, NULL,
                     swap, &args[i])) {
            printf("Could not create thread #%d", i);
            exit(1);
        }
    }

    // Wait for the threads to finish
    for (i = 0; i < opt.num_threads; i++)
        pthread_join(threads[i].thread_id, NULL);

    for (i = 0; i < opt.buffer_size; i++)
    {
        if (pthread_mutex_destroy(&mutexes[i]))
        {
            printf("Can not destroy mutex\n");
            exit(1);
        }
    }

    // Print the buffer
    printf("Buffer after:  ");
    qsort(buffer.data, opt.buffer_size, sizeof(int), (int (*)(const void *, const void *)) cmp);
    print_buffer(buffer);

    printf("iterations: %d\n", get_count());

    free(args);
    free(threads);
    free(buffer.data);
    free(mutexes);

    pthread_exit(NULL);
}

int main (int argc, char **argv)
{
    struct options opt;

    // Default values for the options
    opt.num_threads = 10;
    opt.buffer_size = 10;
    opt.iterations  = 100;
    opt.delay       = 10;

    read_options(argc, argv, &opt);

    start_threads(opt);

    exit (0);
}
