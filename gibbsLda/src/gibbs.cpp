/**
 * @author Yuncheng Li (raingomm[AT]gmail.com)
 * @version 2014/04/23
 */

#include <Rcpp.h>
#include <algorithm>
//#include <gsl/gsl_rng.h>
//#include <gsl/gsl_randist.h>

#include <cstdlib>

using namespace Rcpp;  // just to be explicit

double randMToN(double M, double N)
{
    return M + (rand() / ( RAND_MAX / (N-M) ) ) ;
}

//size_t discrete_sample(gsl_rng *r, double * P, size_t K)
size_t discrete_sample(double * P, size_t K)
{
    size_t i = 0;
    for (i = 1; i < K; i++) {
        P[i] += P[i - 1];
    }
    //double alpha = gsl_ran_flat(r, 0.0, P[K - 1]);
    double alpha = randMToN(0.0, P[K - 1]);
    double *target = std::upper_bound(P, P + K, alpha);
    return (size_t)(target - P);
}

// [[Rcpp::export]]
IntegerVector table_1d_fast(IntegerVector input, int n)
{
    IntegerVector res(n);

    int n_pairs = input.size(), i;
    for (i = 0; i < n_pairs; i++) {
        ++res(input(i));
    }
    return res;
}

// [[Rcpp::export]]
IntegerMatrix table_2d_fast(IntegerVector lhs, IntegerVector rhs, int n_lhs, int n_rhs)
{
    IntegerMatrix res(n_lhs, n_rhs);
    int n_pairs = lhs.size(), i;
    for (int i = 0; i < n_pairs; i++) {
        ++res(lhs(i), rhs(i));
    }
    return res;
}

// [[Rcpp::export]]
IntegerVector gibbs_lda_c(IntegerMatrix docs, IntegerMatrix Ndk, IntegerMatrix Nwk, IntegerVector Nk, IntegerVector Z, size_t K, int niter = 20, double beta = .05, double alpha = .01)
{
    size_t n_pairs = docs.nrow();
    size_t i, j, k;
    double *P;
    //gsl_rng *r = gsl_rng_alloc(gsl_rng_mt19937);

    size_t n_words = Nwk.nrow();

    GetRNGstate();

    P = new double[K];

    for (i = 0; i < niter; i++) {
        for (j = 0; j < n_pairs; j++) {

            size_t doc_id = docs(j, 0);
            size_t word_id = docs(j, 1);
            size_t cnt = docs(j, 2);
            size_t topic = Z(j);

            --Ndk(doc_id, topic);
            --Nwk(word_id, topic);
            --Nk(topic);

            for (k = 0; k < K; k++) {
                P[k] = (Ndk(doc_id, k) + alpha) * (Nwk(word_id, k) + beta) / (Nk(k) + beta * n_words);
            }


            //Z(j) = discrete_sample(r, P, K);
            Z(j) = discrete_sample(P, K);
            topic = Z(j);

            ++Ndk(doc_id, topic);
            ++Nwk(word_id, topic);
            ++Nk(topic);
        }
        break;
    }

    PutRNGstate();

    //gsl_rng_free(r);
    delete [] P;
    return Z;
}
// [[Rcpp::depends(RcppGSL)]]
