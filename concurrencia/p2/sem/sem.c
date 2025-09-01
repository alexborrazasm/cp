#include <errno.h>
#include "sem.h"
int sem_init(sem_t *s, int value)
{   
    // Initial value can be negative
    if (value < 0)
    {
        return -1;
    }
    
    s->count = value;

    int result;

    // Initialize the cond_t
    result = pthread_cond_init(&s->waiting, NULL);
    if (result) {
        return result;
    }

    result = pthread_mutex_init(&s->m, NULL);
    if (result) {
        // Clean up the cond_t
        pthread_cond_destroy(&s->waiting);
        return result;
    }
    
    return 0;  // Success
}

int sem_destroy(sem_t *s)
{
    int result;

    // Destroy the cond_t
    result = pthread_cond_destroy(&s->waiting);
    if(result) {
        return result;
    }

    // Destroy the mutex
    result = pthread_mutex_destroy(&s->m);
    if(result) {
        return result;
    }
    
    return 0;  // Success
}

int sem_p(sem_t *s)
{   
    pthread_mutex_lock(&s->m);
    
    while (s->count == 0) { 
        pthread_cond_wait(&s->waiting, &s->m);
    }

    s->count--;

    pthread_mutex_unlock(&s->m);
    
    return 0;  // Success
}

int sem_tryp(sem_t *s) // 0 on success, -1 if already locked
{   
    pthread_mutex_lock(&s->m);

    if (s->count == 0) {
        pthread_mutex_unlock(&s->m);
        return -1; // already locked
    }

    s->count--;

    pthread_mutex_unlock(&s->m);

    return 0;  // Success
}

int sem_v(sem_t *s)
{   
    pthread_mutex_lock(&s->m);

    s->count++;

    pthread_cond_signal(&s->waiting);

    pthread_mutex_unlock(&s->m);

    return 0;  // Success
}