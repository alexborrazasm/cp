#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include "it_count.h"

struct IterationCounter {
    int iterations;
    pthread_mutex_t mutex;
};

IterationCounter* create_counter(int initial_it)
{
    IterationCounter* counter = malloc(sizeof(IterationCounter));
    if (counter == NULL)
        return NULL;
    
    counter->iterations = initial_it;
    if (pthread_mutex_init(&counter->mutex, NULL) != 0) {
        free(counter);
        return NULL;
    }
    
    return counter;
}

void destroy_counter(IterationCounter **counter)
{
    if (*counter) {
        pthread_mutex_destroy(&(*counter)->mutex);
        free(*counter);
        *counter = NULL;
    }
}

bool do_it(IterationCounter* counter) 
{
    bool result = false;
    pthread_mutex_lock(&counter->mutex);

    if (counter->iterations > 0) {
        --counter->iterations;
        result = true;
    }

    pthread_mutex_unlock(&counter->mutex);
    return result;
}
