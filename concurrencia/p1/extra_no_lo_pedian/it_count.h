#ifndef IT_COUNT_H
#define IT_COUNT_H

#include <stdbool.h>

typedef struct IterationCounter IterationCounter;

IterationCounter* create_counter(int initial_count);
void destroy_counter(IterationCounter* counter);
void set_it(IterationCounter* counter, int i);
bool do_it(IterationCounter* counter);
int get_it(IterationCounter* counter);

#endif // IT_COUNT_H