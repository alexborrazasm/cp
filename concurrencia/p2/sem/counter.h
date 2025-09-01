#ifndef IT_COUNT_H
#define IT_COUNT_H

typedef struct counter_t counter_t;

counter_t* create_counter(int init);
void destroy_counter(counter_t **counter);
void count(counter_t *counter); 
int get_counter(counter_t *counter);

#endif // IT_COUNT_H
