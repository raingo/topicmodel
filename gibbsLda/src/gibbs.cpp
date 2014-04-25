/**
 * @author Yuncheng Li (raingomm[AT]gmail.com)
 * @version 2014/04/23
 */

//#include <Rcpp.h>
#include <cstdlib>
#include <Rmath.h>
#include <algorithm>
#include <fstream>

#include <RcppArmadillo.h>

using namespace Rcpp;

size_t discrete_sample(double * P, size_t K)
{
    size_t i = 0;
    for (i = 1; i < K; i++) {
        P[i] += P[i - 1];
    }
    double alpha = Rf_runif(0.0, P[K - 1]);
    double *target = std::upper_bound(P, P + K, alpha);
    return (size_t)(target - P);
}

// [[Rcpp::export]]
IntegerVector table_1d_fast(IntegerVector input, int n)
{
    IntegerVector res(n);

    int n_pairs = input.size();
    for (int i = 0; i < n_pairs; i++) {
        ++res(input(i));
    }
    return res;
}

// [[Rcpp::export]]
IntegerMatrix table_2d_fast(IntegerVector lhs, IntegerVector rhs, int n_lhs, int n_rhs)
{
    IntegerMatrix res(n_lhs, n_rhs);
    int n_pairs = lhs.size();
    for (int i = 0; i < n_pairs; i++) {
        ++res(lhs(i), rhs(i));
    }
    return res;
}

arma::uvec gen_uvec(IntegerVector src)
{
    arma::uvec dst(src.size());
    int i = 0;
    for (i = 0; i < src.size(); i++) {
        dst(i) = (arma::uword)src(i);
    }
    return dst;
}

// [[Rcpp::export]]
List gibbs_lda_c(IntegerMatrix docs, IntegerMatrix Ndk, IntegerMatrix Nwk, IntegerVector Nk, IntegerVector Z, IntegerVector doc_list, IntegerVector word_list, size_t K, size_t niter = 20, double beta = .05, double alpha = .01)
{
    size_t n_pairs = docs.nrow();
    size_t i, j, k;
    double *P;

    size_t n_words = Nwk.nrow();

    //Rprintf("IN the function now!\n");

    arma::icube doc_trace(doc_list.size(), K, niter);
    arma::icube word_trace(word_list.size(), K, niter);
    arma::imat Nk_trace(K, niter);

    arma::icolvec m_Z(Z.begin(), Z.size(), false);
    arma::imat m_Ndk(Ndk.begin(), Ndk.nrow(), Ndk.ncol(), false);
    arma::imat m_Nwk(Nwk.begin(), Nwk.nrow(), Nwk.ncol(), false);
    arma::icolvec m_Nk(Nk.begin(), Nk.size(), false);

    arma::uvec m_doc_list = gen_uvec(doc_list);
    arma::uvec m_word_list = gen_uvec(word_list);

    GetRNGstate();

    P = new double[K];

    for (i = 0; i < niter; i++) {
        for (j = 0; j < n_pairs; j++) {

            size_t doc_id = docs(j, 0);
            size_t word_id = docs(j, 1);
            //size_t cnt = docs(j, 2);
            size_t topic = m_Z(j);

            --m_Ndk(doc_id, topic);
            --m_Nwk(word_id, topic);
            --m_Nk(topic);

            for (k = 0; k < K; k++) {
                P[k] = (m_Ndk(doc_id, k) + alpha) * (m_Nwk(word_id, k) + beta) / (m_Nk(k) + beta * n_words);
            }

            m_Z(j) = discrete_sample(P, K);
            topic = m_Z(j);

            ++m_Ndk(doc_id, topic);
            ++m_Nwk(word_id, topic);
            ++m_Nk(topic);
        }
        Nk_trace.col(i) = m_Nk;
        doc_trace.slice(i) = m_Ndk.rows(m_doc_list);
        word_trace.slice(i) = m_Nwk.rows(m_word_list);
    }

    PutRNGstate();

    delete [] P;

    return Rcpp::List::create(Rcpp::Named("doc.trace") = doc_trace,
                          Rcpp::Named("word.trace") = word_trace,
                          Rcpp::Named("nk.trace") = Nk_trace
                          );
}

// [[Rcpp::depends(RcppArmadillo)]]
