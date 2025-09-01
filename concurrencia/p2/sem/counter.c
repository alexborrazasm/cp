#include <pthread.h>
#include <stdlib.h>
#include "counter.h"

struct counter_t {
    int n;
    pthread_mutex_t mutex;
};

counter_t* create_counter(int init)
{
    counter_t *counter = malloc(sizeof(struct counter_t));
    if (counter == NULL)
        return NULL;

    counter->n = init;
    
    if (pthread_mutex_init(&counter->mutex, NULL) != 0) {
        free(counter);
        return NULL;
    }
    
    return counter;
}

void destroy_counter(counter_t **counter)
{
    if (*counter) {
        pthread_mutex_destroy(&(*counter)->mutex);
        free(*counter);
        *counter = NULL;
    }
}

void count(counter_t *counter) 
{
    pthread_mutex_lock(&counter->mutex);
    ++counter->n;    
    pthread_mutex_unlock(&counter->mutex);
}

int get_counter(counter_t *counter)
{
    int n;

    pthread_mutex_lock(&counter->mutex);
    n = counter->n; 
    pthread_mutex_unlock(&counter->mutex);

    return n;
}