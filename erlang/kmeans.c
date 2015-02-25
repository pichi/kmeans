#include <erl_nif.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

static inline double
distance(const double *a, const double *b, const unsigned size) {
    double distance = 0.0;
    for (unsigned i = 0; i<size; i++) {
        const double d = a[i]-b[i];
        distance += d*d;
    };
    return distance;
}

static int
points_bin(ErlNifEnv *env, ERL_NIF_TERM term, const unsigned size, const double **data, unsigned *length){
    ErlNifBinary bin;
    const unsigned point_size = size*sizeof(double);
    if (!(
                enif_inspect_binary(env, term, &bin)
                && bin.size > 0
                && (bin.size % point_size) == 0
         )) return 0;
    *data = (const double *)bin.data;
    *length = bin.size / point_size;
    return 1;
}

static ERL_NIF_TERM step(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned s = 0;
    const double *data, *centroids;
    double *new;
    unsigned l, c;
    ERL_NIF_TERM result;

    if (!(
                enif_get_uint(env, argv[0], &s) &&
                s > 0 &&
                points_bin(env, argv[1], s, &data, &l) &&
                l > 0 &&
                points_bin(env, argv[2], s, &centroids, &c) &&
                c > 0 && c < l &&
                (new = (double *)enif_make_new_binary(env, c*s*sizeof(double), &result))
         )) return enif_make_badarg(env);

    // clear new centroids
    memset(new, 0, c*s*sizeof(double));

    // declarations
    const unsigned size = s, count = c, len = l;
    unsigned *counts = (unsigned *)enif_alloc(count * sizeof(unsigned));
    if (!counts) return enif_make_badarg(env);
    memset(counts, 0, count * sizeof(unsigned));

    // loop over points (xs) and set states
    double sum_d = 0.0;
    for (unsigned i=0; i<len; i++) {
        // find minimum
        unsigned min_i = 0;
        const double *point = data + i*size;
        double min_d = distance(point, centroids, size);
        for (unsigned j = 1; j<count; j++) {
            const double d = distance(point, centroids + j*size, size);
            if (d<min_d) { min_d = d; min_i = j; };
        }

        // update state
        sum_d += min_d;
        counts[min_i]++;
        for (unsigned j=0; j<size; j++) new[min_i*size+j] += point[j];
    }

    // prepare new centroids (average)
    for (unsigned i=0; i<count; i++) {
        if (counts[i]) {
            const double n = counts[i];
            for (unsigned j=0; j<size; j++)
                new[i*size+j] /= n;
        }
    }

    // add distance info
    result = enif_make_tuple2(env, result, enif_make_double(env, sum_d));

    enif_free(counts);
    return result;
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

static ERL_NIF_TERM points2bin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    ERL_NIF_TERM list = argv[0];
    unsigned len = 0;
    int s = 0;
    const ERL_NIF_TERM *array;
    ERL_NIF_TERM head, result;
    double *data;

    if (!(
                enif_get_list_length(env, list, &len) &&
                len > 0 &&
                enif_get_list_cell(env, list, &head, &result) &&
                enif_get_tuple(env, head, &s, &array) &&
                s > 0 &&
                (data = (double *)enif_make_new_binary(env, len*s*sizeof(double), &result))
         )) return enif_make_badarg(env);

    const unsigned size = s;
    for (unsigned i=0; enif_get_list_cell(env, list, &head, &list); i++) {
        if (!get_point(env, head, size, data + i*size))
            return enif_make_badarg(env);
    }

    return result;
}

static inline ERL_NIF_TERM
make_point(ErlNifEnv* env, const double *d, const unsigned size) {
    ERL_NIF_TERM array[size];
    for (unsigned i=0; i<size; i++)
        array[i] = enif_make_double(env, d[i]);
    return enif_make_tuple_from_array(env, array, size);
}

static ERL_NIF_TERM bin2points(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned s = 0, l = 0;
    const double *data;

    if (!(
                enif_get_uint(env, argv[0], &s) &&
                s > 0 &&
                points_bin(env, argv[1], s, &data, &l) &&
                l > 0
         )) return enif_make_badarg(env);

    ERL_NIF_TERM list = enif_make_list(env, 0);
    const unsigned size = s;
    unsigned count = l;
    for (; count; count--) {
        list = enif_make_list_cell(env, make_point(env, data + (count-1)*size, size), list);
    }

    return list;
}

static ErlNifFunc nif_funcs[] = {
    {"bin2points", 2, bin2points},
    {"points2bin", 1, points2bin},
    {"step", 3, step}
};

#pragma GCC visibility push(default)
ERL_NIF_INIT(kmeans, nif_funcs, NULL, NULL, NULL, NULL)
#pragma GCC visibility pop
