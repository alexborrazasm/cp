#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include "options.h"
#include "sem.h"
#include "counter.h"

#define RST    "\033[0m"
#define RED    "\033[1m\033[31m"
#define GREEN  "\033[1m\033[32m"
#define YELLOW "\033[1m\033[33m"
#define BLUE   "\033[1m\033[34m"

struct thread_info {
    pthread_t thread_id;       // id returned by pthread_create()
    int       thread_num;      // application defined thread #
};

struct args_barber {
    int             thread_num;         // application defined thread #
    int             cut_time;           // delay between cut hair
    sem_t           *barber;
    sem_t           *customers;
    sem_t           *free_seats_s;
    counter_t       *counter;
    bool            *done;
    pthread_mutex_t *done_m;
};

struct args_customer {
    int       thread_num;         // application defined thread #
    int       cut_time;           // delay between cut hair
    int       wait_time;
    sem_t     *barber;
    sem_t     *customers;
    sem_t     *free_seats_s;
    counter_t *cut_c;
    counter_t *leave_c;
};

void cutHair(int id, int delay)
{
    printf("  Barber: "YELLOW"%4d"BLUE" do cut hair\n"RST, id);
    usleep(delay);
}

void getHairCut(int id, int delay)
{
    printf("Customer: "YELLOW"%4d "GREEN"cut hair\n"RST, id);
    usleep(delay);
}

void leave(int id)
{
    printf("Customer: "YELLOW"%4d "RED"left\n"RST, id);
}

void *barber(void *ptr)
{
    struct args_barber *args = ptr;
    
    while (1)
    {
        sem_p(args->customers);  // wait for customers
        sem_v(args->barber);     // wake up a customer

        pthread_mutex_lock(args->done_m);
        if (*args->done)
        {
            pthread_mutex_unlock(args->done_m);
            break; 
        }
        pthread_mutex_unlock(args->done_m);
        cutHair(args->thread_num, args->cut_time);
        count(args->counter);
    }

    return NULL;
}

void *customer(void *ptr)
{
    struct args_customer *args = ptr;

    // so that they don't all arrive at once
    usleep(args->thread_num * args->wait_time);
    
    if (!sem_tryp(args->free_seats_s)) // free seats?
    {
        sem_v(args->customers);    // customers++
        sem_p(args->barber);       // wait for barber
        sem_v(args->free_seats_s); // seats++

        getHairCut(args->thread_num, args->cut_time);
        count(args->cut_c);
    }
    else // he is leaving
    {
        leave(args->thread_num);
        count(args->leave_c);
    }
   
    return NULL;
}

void startThr(struct options opt)
{
    int i;
    struct thread_info *thrBarbers, *thrCustomers;
    struct args_barber *argsBarbers;
    struct args_customer *argsCustomers;
    sem_t *sBarber, *sCustomer, *sFreeSeats;
    counter_t *cutCounter, *leaveCounter, *barberCounter;
    bool *done;
    pthread_mutex_t *doneM;
    
    // Reserve memory to semaphores
    sBarber    = malloc(sizeof(sem_t));
    sCustomer  = malloc(sizeof(sem_t));
    sFreeSeats = malloc(sizeof(sem_t));

    if (!sBarber || !sCustomer || !sFreeSeats)
    {
        printf("Out of memory\n");
        exit(1); 
    }

    // Initiate semaphores
    if (sem_init(sBarber, 0) || sem_init(sCustomer, 0) || 
        sem_init(sFreeSeats, opt.max_seats))
    {
        printf("Can not initiate semaphores\n");
        exit(1); 
    }
    
    // Initiate thread_info for barbers and costumers
    thrBarbers    = malloc(sizeof(struct thread_info) * opt.barbers);
    thrCustomers  = malloc(sizeof(struct thread_info) * opt.customers);
    argsBarbers   = malloc(sizeof(struct args_barber) * opt.barbers);    
    argsCustomers = malloc(sizeof(struct args_customer) * opt.customers);

    if (!thrBarbers || !thrCustomers || !argsBarbers || !argsCustomers)
    {
        printf("Out of memory\n");
        exit(1); 
    }

    done  = malloc(sizeof(bool));
    doneM = malloc(sizeof(pthread_mutex_t));

    if (!done || !doneM)
    {
        printf("Out of memory\n");
        exit(1); 
    }
    
    *done = false;
    if (pthread_mutex_init(doneM, NULL)) {
        printf("Can not initiate mutex\n");
        exit(1);
    }

    cutCounter    = create_counter(0);
    leaveCounter  = create_counter(0);
    barberCounter = create_counter(0);

    if (!cutCounter || !leaveCounter || !barberCounter) {
        printf("Can not initiate counters\n");
        exit(1);
    }
    
    // Create opt.barbers threads running barber()
    for (i = 0; i < opt.barbers; i++)
    {
        thrBarbers[i].thread_num = i;
        argsBarbers[i].thread_num = i;
        argsBarbers[i].barber = sBarber;
        argsBarbers[i].customers = sCustomer;
        argsBarbers[i].free_seats_s = sFreeSeats;
        argsBarbers[i].cut_time = opt.cut_time;
        argsBarbers[i].counter = barberCounter;
        argsBarbers[i].done = done;
        argsBarbers[i].done_m = doneM;

        if ( 0 != pthread_create(&thrBarbers[i].thread_id, NULL, barber,
             &argsBarbers[i]))
        {
            printf("Could not create thread #%d\n", i);
            exit(1);
        }
    } 

    // Create opt.customers threads running customers()
    for (i = 0; i < opt.customers; i++)
    {
        thrCustomers[i].thread_num = i;
        argsCustomers[i].thread_num = i;
        argsCustomers[i].barber = sBarber;
        argsCustomers[i].customers = sCustomer;
        argsCustomers[i].free_seats_s = sFreeSeats;
        argsCustomers[i].cut_time = opt.cut_time;
        argsCustomers[i].leave_c = leaveCounter;
        argsCustomers[i].cut_c = cutCounter;
        argsCustomers[i].wait_time = opt.customer_wait;

        if ( 0 != pthread_create(&thrCustomers[i].thread_id, NULL, customer,
             &argsCustomers[i]))
        {
            printf("Could not create thread #%d\n", i);
            exit(1);
        }
    }

    // wait for the customers threads to finish
    for (i = 0; i < opt.customers; i++)
        pthread_join(thrCustomers[i].thread_id, NULL);

    pthread_mutex_lock(doneM);
    *done = true;
    pthread_mutex_unlock(doneM);

    for (int i = 0; i < opt.barbers; i++)
    {
        // Wake up barbers
        sem_v(sCustomer);
    }

    // wait for barbers threads to finish
    for (i = 0; i < opt.barbers; i++)
        pthread_join(thrBarbers[i].thread_id, NULL); 

    printf(
        "-----------------SETINGS-----------------\n"
        "Total   barbers -> "YELLOW"%5d\n"RST
        "Total customers -> "YELLOW"%5d\n"RST
        "Total    chairs -> "YELLOW"%5d\n"RST
        " Wait customers -> "YELLOW"%5d\n"RST
        "-----------------RESULTS-----------------\n"
        "    Barber        cut -> "YELLOW"%5d\n"RST
        " Customers        cut -> "YELLOW"%5d\n"RST
        " Customers       left -> "YELLOW"%5d\n"RST
        " Customers cut + left -> "YELLOW"%5d\n"RST
        "-------------------END-------------------\n",
        opt.barbers,
        opt.customers,
        opt.customer_wait,
        opt.customer_wait,
        get_counter(barberCounter),
        get_counter(cutCounter),
        get_counter(leaveCounter),
        get_counter(cutCounter) + get_counter(leaveCounter)
    );
    
    // Free allocate resources
    destroy_counter(&cutCounter);
    destroy_counter(&leaveCounter);
    destroy_counter(&barberCounter);

    sem_destroy(sBarber);
    sem_destroy(sCustomer);
    sem_destroy(sFreeSeats);

    pthread_mutex_destroy(doneM);

    free(done);
    free(doneM);
    free(sBarber);
    free(sCustomer);
    free(sFreeSeats);
    free(thrBarbers);
    free(thrCustomers);
    free(argsBarbers);
    free(argsCustomers);
}

int main(int argc, char **argv)
{
    struct options opt;

    // Default values for the options
    opt.barbers       = 5;
    opt.customers     = 1000;
    opt.cut_time      = 3000;
    opt.max_seats     = 5;
    opt.customer_wait = 400;

    read_options(argc, argv, &opt);

    startThr(opt);

    return 0;
}
