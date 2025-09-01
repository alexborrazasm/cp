#include "rec_mutex.h"

int rec_mutex_init(rec_mutex_t *m)
{
    int result;

    m->count = 0;

    result = pthread_mutex_init(&m->mutex, NULL);
    if (result) {
        return result;
    }

    result = pthread_cond_init(&m->wait, NULL);
    if (result) {
        pthread_mutex_destroy(&m->mutex);
        return result;
    }
    
    return 0; // success
}

int rec_mutex_destroy(rec_mutex_t *m)
{
    int result;

    result = pthread_cond_destroy(&m->wait); 
    if (result) {
        return result;
    }

    return pthread_mutex_destroy(&m->mutex);
}

int rec_mutex_lock(rec_mutex_t *m)
{
    pthread_t this = pthread_self();

    pthread_mutex_lock(&m->mutex);

    if (m->count) // mutex is already locked
    {
        // If the two thread IDs are equal returns a nonzero
        if (pthread_equal(this, m->id)) // same thread, recursive lock
        {
            m->count++;
            pthread_mutex_unlock(&m->mutex);
            return 0;
        }
        else  // different threads, must wait
        {
            while (m->count)
            {
                pthread_cond_wait(&m->wait, &m->mutex);
            }
        }
    }
    
    // not locked, so lock it
    m->id = this; 
    m->count = 1;
    
    pthread_mutex_unlock(&m->mutex);

    return 0;
}

int rec_mutex_trylock(rec_mutex_t *m)
{
    pthread_t this = pthread_self();

    pthread_mutex_lock(&m->mutex);

    if (m->count) // mutex is already locked
    {
        // If the two thread IDs are equal returns a nonzero
        if (pthread_equal(this, m->id)) // same thread, recursive lock
        {
            m->count++;
            pthread_mutex_unlock(&m->mutex);
            return 0;
        }
        else // different thread can't lock
        {
            pthread_mutex_unlock(&m->mutex);
            return -1;
        }
    }

    // not locked, so lock it
    m->id = this; 
    m->count = 1;
    
    pthread_mutex_unlock(&m->mutex);

    return 0;
}

int rec_mutex_unlock(rec_mutex_t *m)
{   
    pthread_mutex_lock(&m->mutex);

    // If the two thread IDs are equal returns a nonzero
    if (!pthread_equal(pthread_self(), m->id) || m->count == 0)
    {
        pthread_mutex_unlock(&m->mutex);
        return -1;
    }

    if (--m->count == 0) // lock is fully released
    {    
        // Signal one waiting thread (if any) that the lock is now available
        pthread_cond_signal(&m->wait);
    }

    pthread_mutex_unlock(&m->mutex);
    
    return 0;
}
