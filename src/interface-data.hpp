#ifndef INTERFACE_DATA_H
#define INTERFACE_DATA_H

#include <Rcpp.h>
#include <survival.hpp>

RcppExport SEXP load_recapture_data(SEXP x);
RcppExport SEXP get_N_data(SEXP xp);
RcppExport SEXP get_K_data(SEXP xp);
RcppExport SEXP get_recaptures_data(SEXP xp, SEXP id);
RcppExport SEXP get_surveys_data(SEXP xp);
RcppExport SEXP get_births_data(SEXP xp);
RcppExport SEXP get_first_obs_data(SEXP xp);
RcppExport SEXP get_last_obs_data(SEXP xp);
RcppExport SEXP get_sampled_data(SEXP xp);

#endif
