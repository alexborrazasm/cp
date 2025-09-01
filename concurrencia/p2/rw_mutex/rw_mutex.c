#include "rw_mutex.h"

int rw_mutex_init(rw_mutex_t *m)
{
    m->readers = 0;

    int result;

    // Initialize the exclusion mutex
    result = pthread_mutex_init(&m->excl_m, NULL);
    if (result) {
        return result;
    }

    // Initialize the readers mutex
    result = pthread_mutex_init(&m->readers_m, NULL);
    if (result) {
        // Clean up the exclusion mutex if readers mutex initialization fails
        pthread_mutex_destroy(&m->excl_m);
        return result;
    }

    return 0;  // Success
}

int rw_mutex_destroy(rw_mutex_t *m)
{
    int result;

    // Destroy the exclusion mutex
    result = pthread_mutex_destroy(&m->excl_m);
    if (result) {
        return result;
    }

    // Destroy the readers mutex
    return pthread_mutex_destroy(&m->readers_m);
}

int rw_mutex_writelock(rw_mutex_t *m)
{   
    // Simple locks exclusive mutex
    return pthread_mutex_lock(&m->excl_m);
}

int rw_mutex_writeunlock(rw_mutex_t *m)
{
    // Unlock exclusive mutex
    return pthread_mutex_unlock(&m->excl_m);
}

int rw_mutex_readlock(rw_mutex_t *m)
{
    pthread_mutex_lock(&m->readers_m); // Lock readers mutex to readers++

    if (++m->readers == 1) { // Only the first reader
        pthread_mutex_lock(&m->excl_m); // Lock exclusive mutex
    }
    
    pthread_mutex_unlock(&m->readers_m);  // Unlock readers mutex

    return 0;  // Success
}

int rw_mutex_readunlock(rw_mutex_t *m)
{
    pthread_mutex_lock(&m->readers_m); // Lock readers mutex to readers--

    if (--m->readers == 0) { // Only the last reader
        pthread_mutex_unlock(&m->excl_m); // Unlock exclusive mutex
    }
    
    pthread_mutex_unlock(&m->readers_m);  // Unlock readers mutex

    return 0;  // Success
}
