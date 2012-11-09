#ifndef INTERFACE_LIKELIHOOD_H
#define INTERFACE_LIKELIHOOD_H

#include <Rcpp.h>
#include <survival_FLAT.hpp>

RcppExport SEXP load_recapture_likelihood(SEXP x);
RcppExport SEXP get_N_likelihood(SEXP xp);
RcppExport SEXP get_K_likelihood(SEXP xp);
RcppExport SEXP get_recaptures_likelihood(SEXP xp, SEXP id);
RcppExport SEXP get_surveys_likelihood(SEXP xp);
RcppExport SEXP get_births_likelihood(SEXP xp);
RcppExport SEXP get_first_obs_likelihood(SEXP xp);
RcppExport SEXP get_last_obs_likelihood(SEXP xp);
RcppExport SEXP get_sampled_likelihood(SEXP xp);
RcppExport SEXP get_deaths_likelihood(SEXP xp);
RcppExport SEXP set_deaths_likelihood(SEXP xp, SEXP id, SEXP td);

RcppExport SEXP get_PHI_likelihood(SEXP xp);
RcppExport SEXP get_P_likelihood(SEXP xp);
RcppExport SEXP set_PHI_likelihood(SEXP xp, SEXP PHI_);
RcppExport SEXP set_P_likelihood(SEXP xp, SEXP P_);

RcppExport SEXP get_ll_phi_components_likelihood(SEXP xp);
RcppExport SEXP get_ll_p_components_likelihood(SEXP xp);
RcppExport SEXP get_some_ll_phi_components_likelihood(SEXP xp, SEXP indices);
RcppExport SEXP get_some_ll_p_components_likelihood(SEXP xp, SEXP indices);

#endif
