#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include "op_count.h"
#include "options.h"
#include "it_count.h"
#include <string.h>

#define RST    "\033[0m"
#define RED    "\033[1m\033[31m"
#define GREEN  "\033[1m\033[32m"
#define BLUE   "\033[1m\033[34m"

struct buffer {
    int *data;
    int size;
};

struct thread_info {
    pthread_t       thread_id;        // id returned by pthread_create()
    int             thread_num;       // application defined thread #
};

struct print_thr_info {
    pthread_t       thread_id;
};

struct args {
    int				 thread_num;       // application defined thread #
    int				 delay;			   // delay between operations
    int				 iterations;
    struct buffer	 *buffer;		   // Shared buffer
    pthread_mutex_t  *mutexes;         // mutexes array
    IterationCounter *it_counter;      // global it counter
};

struct print_args {
    struct buffer    *buffer;
    pthread_mutex_t  *print_mutex;
    pthread_mutex_t  *mutexes;
    int              print_wait;       // Delay between prints
    IterationCounter *it_counter;      // global it counter
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
    struct args *args = ptr;

    while(do_it(args->it_counter)) 
    {
        int i, j, tmp;
        i = rand() % args->buffer->size;
        j = rand() % args->buffer->size;

        lock_positions(args->mutexes, i, j);

        printf("Thread "GREEN"%d"RST" swapping positions %d (== %d) and %d (== %d)\n",
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
    int i; char *msg;    

    msg = malloc(sizeof(char) * 12 * buffer.size + 1); // 11 min_int 1 ' '
    *msg = '\0';

    if (!msg) {
        printf("Out of memory\n");
        exit(1);
    }
    
    for (i = 0; i < buffer.size; i++)
    {
        char tmp[12];
        sprintf(tmp," %d", buffer.data[i]);
        strcat(msg, tmp);
    }

    printf(BLUE"[%s ]\n"RST, msg);
    free(msg);
}

bool finish(bool *should_terminate, pthread_mutex_t *m)
{
    pthread_mutex_lock(m);
    if (*should_terminate) 
    {
        pthread_mutex_unlock(m);
        return false;
    }
    else
    {
        pthread_mutex_unlock(m);
        return true;
    }   
}

void *print(void *ptr)
{
    struct print_args *args = ptr;
    struct buffer bufferCopy;
    bufferCopy.size = args->buffer->size;
    bufferCopy.data = malloc(sizeof(int) * bufferCopy.size);
    
    while (get_it(args->it_counter) != 0)
    {
        int i;
        
        // Lock all mutexes
        for (i = 0; i < args->buffer->size; i++)
            pthread_mutex_lock(&args->mutexes[i]);
        
        // Copy buffer
        for (i = 0; i < bufferCopy.size; i++)
            bufferCopy.data[i] = args->buffer->data[i];

        // Unlock buffer to other thr
        for (i = 0; i < args->buffer->size; i++)
            pthread_mutex_unlock(&args->mutexes[i]);
        
        // Sort copy
        qsort(bufferCopy.data, bufferCopy.size, sizeof(int),
            (int (*)(const void *, const void *)) cmp);
        
        // Print copy
        printf("Current buffer state: ");
        print_buffer(bufferCopy);

        usleep(args->print_wait);
    }

    free(bufferCopy.data);
    
    return NULL;
}

void start_threads(struct options opt)
{
    int i;
    struct thread_info *threads;
    struct print_thr_info *printThread;
    struct args *args;
    struct print_args *printArgs;
    struct buffer buffer;
    pthread_mutex_t *mutexes;
    IterationCounter *itCounter;

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
        pthread_mutex_init(&mutexes[i], NULL);
    }

    printf("creating %d threads\n", opt.num_threads);
    threads = malloc(sizeof(struct thread_info) * opt.num_threads);
    args = malloc(sizeof(struct args) * opt.num_threads);

    if (threads == NULL || args == NULL || mutexes == NULL) {
        printf("Not enough memory\n");
        exit(1);
    }

    printf("Buffer before: ");
    print_buffer(buffer);

    // Init global counter 
    itCounter = create_counter(opt.iterations);

    // Create num_thread threads running swap()
    for (i = 0; i < opt.num_threads; i++) {
        threads[i].thread_num = i;

        args[i].thread_num = i;
        args[i].buffer     = &buffer;
        args[i].delay      = opt.delay;
        args[i].iterations = opt.iterations;
        args[i].mutexes    = mutexes;
        args[i].it_counter = itCounter;

        if ( 0 != pthread_create(&threads[i].thread_id, NULL,
                     swap, &args[i])) {
            printf("Could not create thread #%d", i);
            exit(1);
        }
    }

    printThread = malloc(sizeof(struct print_thr_info));
    printArgs = malloc(sizeof(struct print_args));

    if (!printThread || !printArgs) {
        printf("Not enough memory\n");
        exit(1);
    }

    printArgs->buffer = &buffer;
    printArgs->print_wait = opt.print_wait;
    printArgs->mutexes = mutexes;
    printArgs->it_counter = itCounter;

    if (0 != pthread_create(&printThread->thread_id, NULL, print, printArgs)) {
        printf("Not enough memory\n");
        exit(1);
    }
    
    // Wait for the threads to finish
    for (i = 0; i < opt.num_threads; i++)
        pthread_join(threads[i].thread_id, NULL);

    pthread_join(printThread->thread_id, NULL);  // Wait

    // Destroy global counter
    destroy_counter(itCounter);

    for (i = 0; i < opt.buffer_size; i++)
        pthread_mutex_destroy(&mutexes[i]);

    // Print the buffer
    printf("Buffer after:  ");
    qsort(buffer.data, opt.buffer_size, sizeof(int), (int (*)(const void *, const void *)) cmp);
    print_buffer(buffer);

    printf("iterations: %d\n", get_count());

    free(args);
    free(threads);
    free(buffer.data);
    free(mutexes);
    free(printArgs);
    free(printThread);

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
    opt.print_wait  = 1000;

    read_options(argc, argv, &opt);

    start_threads(opt);

    exit (0);
}
