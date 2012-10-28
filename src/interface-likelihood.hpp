#ifndef INTERFACE_LIKELIHOOD_H
#define INTERFACE_LIKELIHOOD_H

#include <Rcpp.h>
#include <survival.hpp>

RcppExport SEXP load_recapture_likelihood(SEXP x);
RcppExport SEXP get_N_likelihood(SEXP xp);
RcppExport SEXP get_K_likelihood(SEXP xp);
RcppExport SEXP get_recaptures_likelihood(SEXP xp, SEXP id);
RcppExport SEXP get_surveys_likelihood(SEXP xp);
RcppExport SEXP get_births_likelihood(SEXP xp);
RcppExport SEXP get_first_obs_likelihood(SEXP xp);
RcppExport SEXP get_last_obs_likelihood(SEXP xp);
RcppExport SEXP get_deaths_likelihood(SEXP xp);

#endif
