#ifndef INTERFACE_STATE_H
#define INTERFACE_STATE_H

#include <Rcpp.h>
#include <survival_FLAT.hpp>

RcppExport SEXP load_recapture_state(SEXP x);
RcppExport SEXP get_N_state(SEXP xp);
RcppExport SEXP get_K_state(SEXP xp);
RcppExport SEXP get_recaptures_state(SEXP xp, SEXP id);
RcppExport SEXP get_surveys_state(SEXP xp);
RcppExport SEXP get_births_state(SEXP xp);
RcppExport SEXP get_first_obs_state(SEXP xp);
RcppExport SEXP get_last_obs_state(SEXP xp);
RcppExport SEXP get_sampled_state(SEXP xp);
RcppExport SEXP get_deaths_state(SEXP xp);
RcppExport SEXP set_deaths_state(SEXP xp, SEXP id, SEXP td);

#endif
