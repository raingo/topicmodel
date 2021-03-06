// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// table_1d_fast
IntegerVector table_1d_fast(IntegerVector input, int n);
RcppExport SEXP gibbsLda_table_1d_fast(SEXP inputSEXP, SEXP nSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< IntegerVector >::type input(inputSEXP );
        Rcpp::traits::input_parameter< int >::type n(nSEXP );
        IntegerVector __result = table_1d_fast(input, n);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// table_2d_fast
IntegerMatrix table_2d_fast(IntegerVector lhs, IntegerVector rhs, int n_lhs, int n_rhs);
RcppExport SEXP gibbsLda_table_2d_fast(SEXP lhsSEXP, SEXP rhsSEXP, SEXP n_lhsSEXP, SEXP n_rhsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< IntegerVector >::type lhs(lhsSEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type rhs(rhsSEXP );
        Rcpp::traits::input_parameter< int >::type n_lhs(n_lhsSEXP );
        Rcpp::traits::input_parameter< int >::type n_rhs(n_rhsSEXP );
        IntegerMatrix __result = table_2d_fast(lhs, rhs, n_lhs, n_rhs);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// gibbs_lda_c
List gibbs_lda_c(IntegerMatrix docs, IntegerMatrix Ndk, IntegerMatrix Nwk, IntegerVector Nk, IntegerVector Z, IntegerVector doc_list, IntegerVector word_list, size_t K, size_t niter = 20, double beta = .05, double alpha = .01);
RcppExport SEXP gibbsLda_gibbs_lda_c(SEXP docsSEXP, SEXP NdkSEXP, SEXP NwkSEXP, SEXP NkSEXP, SEXP ZSEXP, SEXP doc_listSEXP, SEXP word_listSEXP, SEXP KSEXP, SEXP niterSEXP, SEXP betaSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< IntegerMatrix >::type docs(docsSEXP );
        Rcpp::traits::input_parameter< IntegerMatrix >::type Ndk(NdkSEXP );
        Rcpp::traits::input_parameter< IntegerMatrix >::type Nwk(NwkSEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type Nk(NkSEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type Z(ZSEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type doc_list(doc_listSEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type word_list(word_listSEXP );
        Rcpp::traits::input_parameter< size_t >::type K(KSEXP );
        Rcpp::traits::input_parameter< size_t >::type niter(niterSEXP );
        Rcpp::traits::input_parameter< double >::type beta(betaSEXP );
        Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP );
        List __result = gibbs_lda_c(docs, Ndk, Nwk, Nk, Z, doc_list, word_list, K, niter, beta, alpha);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpparma_hello_world
arma::mat rcpparma_hello_world();
RcppExport SEXP gibbsLda_rcpparma_hello_world() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        arma::mat __result = rcpparma_hello_world();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpparma_outerproduct
arma::mat rcpparma_outerproduct(const arma::colvec& x);
RcppExport SEXP gibbsLda_rcpparma_outerproduct(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP );
        arma::mat __result = rcpparma_outerproduct(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpparma_innerproduct
double rcpparma_innerproduct(const arma::colvec& x);
RcppExport SEXP gibbsLda_rcpparma_innerproduct(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP );
        double __result = rcpparma_innerproduct(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpparma_bothproducts
Rcpp::List rcpparma_bothproducts(const arma::colvec& x);
RcppExport SEXP gibbsLda_rcpparma_bothproducts(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP );
        Rcpp::List __result = rcpparma_bothproducts(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
