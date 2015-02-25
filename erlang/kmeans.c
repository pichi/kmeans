#include <erl_nif.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

typedef struct avg_state {
    unsigned n;
    double x[];
} avg_state;

static inline size_t avg_state_size(const unsigned size) {
    return sizeof(avg_state) + size * sizeof(double);
}

static inline avg_state *
get_state(avg_state *base, const unsigned size, const unsigned index) {
    return (avg_state *)((char *)base + index*avg_state_size(size));
}

static inline int
get_point(ErlNifEnv* env, ERL_NIF_TERM term, const unsigned size, double *data) {
    const ERL_NIF_TERM *array;
    int arity;
    if (!(
                enif_get_tuple(env, term, &arity, &array) &&
                arity == size
         )) return 0;
    for (unsigned i=0; i<size; i++)
        if (!enif_get_double(env, array[i], data+i)) return 0;
    return 1;
}

static inline double
distance(const double *a, const double *b, const unsigned size) {
    double distance = 0.0;
    for (unsigned i = 0; i<size; i++) {
        const double d = a[i]-b[i];
        distance += d*d;
    };
    return distance;
}

static inline ERL_NIF_TERM
make_point(ErlNifEnv* env, const double *d, const unsigned size) {
    ERL_NIF_TERM array[size];
    for (unsigned i=0; i<size; i++)
        array[i] = enif_make_double(env, d[i]);
    return enif_make_tuple_from_array(env, array, size);
}

static ERL_NIF_TERM step(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    ERL_NIF_TERM xs = argv[0], centroids = argv[1];
    unsigned c_count = 0;
    int c_arity = 0;
    const ERL_NIF_TERM *array;
    ERL_NIF_TERM head, tail;

    if (!(
                enif_get_list_length(env, centroids, &c_count) &&
                c_count > 0 &&
                enif_get_list_cell(env, centroids, &head, &tail) &&
                enif_get_tuple(env, head, &c_arity, &array)
         )) return enif_make_badarg(env);

    // declarations
    const unsigned size = c_arity, c_states = c_count;
    avg_state *states = NULL;
    double *c_data = NULL, *point = NULL;
#define ERROR { centroids = enif_make_badarg(env); goto error; }

    // allocate avg_states
    states  = (avg_state *)enif_alloc(c_states * avg_state_size(size));
    if (!states) ERROR;
    memset(states, 0, c_states * avg_state_size(size));

    // allocate centroids raw data
    const unsigned point_size = size * sizeof(double);
    c_data = (double *)enif_alloc(c_count * point_size);
    if (!c_data) ERROR;

    // load centroids into raw data
    for (unsigned i=0; enif_get_list_cell(env, centroids, &head, &centroids); i++)
        if (!get_point(env, head, size, c_data + i*point_size)) ERROR;

    // allocate working space point
    point = (double *)enif_alloc(point_size);
    if (!point) ERROR;

    // loop over points (xs) and set states
    double sum_d = 0.0;
    for (; enif_get_list_cell(env, xs, &head, &xs); ) {
        if (!get_point(env, head, size, point)) ERROR;

        // find minimum
        unsigned min_i = 0;
        double min_d = distance(point, c_data, size);
        for (unsigned i = 1; i<c_states; i++) {
            const double d = distance(point, c_data + i*point_size, size);
            if (d<min_d) { min_d = d; min_i = i; };
        }

        // update state
        sum_d += min_d;
        avg_state *state = get_state(states, size, min_i);
        state->n++;
        for (unsigned i=0; i<size; i++) state->x[i] += point[i];
    }

    // prepare new centroids
    centroids = enif_make_list(env, 0);
    for (unsigned i=0; i<c_states; i++) {
        avg_state *state = get_state(states, size, i);
        if (state->n)
            for (unsigned j=0; j<size; j++)
                state->x[j] /= state->n;
        centroids = enif_make_list_cell(env, make_point(env, state->x, size), centroids);
    }

    // add distance info
    centroids = enif_make_tuple2(env, centroids, enif_make_double(env, sum_d));

error:
    if (point) enif_free(point);
    if (c_data) enif_free(c_data);
    if (states) enif_free(states);
    return centroids;
}

static ErlNifFunc nif_funcs[] = {
    {"step", 2, step}
};

#pragma GCC visibility push(default)
ERL_NIF_INIT(kmeans, nif_funcs, NULL, NULL, NULL, NULL)
#pragma GCC visibility pop
