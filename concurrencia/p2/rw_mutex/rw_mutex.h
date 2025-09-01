#ifndef __RW_MUTEX_H__
#define __RW_MUTEX_H__

#include <pthread.h>

typedef struct rw_mutex_t {
    int readers;
    pthread_mutex_t readers_m;
    pthread_mutex_t excl_m; 
} rw_mutex_t;

int rw_mutex_init(rw_mutex_t *m);
int rw_mutex_destroy(rw_mutex_t *m);

int rw_mutex_readlock(rw_mutex_t *m);
int rw_mutex_writelock(rw_mutex_t *m);
int rw_mutex_readunlock(rw_mutex_t *m);
int rw_mutex_writeunlock(rw_mutex_t *m);

#endif