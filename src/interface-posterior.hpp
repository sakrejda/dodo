#ifndef INTERFACE_POSTERIOR_H
#define INTERFACE_POSTERIOR_H

#include <Rcpp.h>
#include <survival.hpp>

RcppExport SEXP load_recapture_posterior(SEXP x);
RcppExport SEXP get_N_posterior(SEXP xp);
RcppExport SEXP get_K_posterior(SEXP xp);
RcppExport SEXP get_recaptures_posterior(SEXP xp, SEXP id);
RcppExport SEXP get_surveys_posterior(SEXP xp);
RcppExport SEXP get_births_posterior(SEXP xp);
RcppExport SEXP get_first_obs_posterior(SEXP xp);
RcppExport SEXP get_last_obs_posterior(SEXP xp);
RcppExport SEXP get_sampled_posterior(SEXP xp);
RcppExport SEXP get_deaths_posterior(SEXP xp);
RcppExport SEXP set_deaths_posterior(SEXP xp, SEXP id, SEXP td);

RcppExport SEXP get_PHI_posterior(SEXP xp);
RcppExport SEXP get_P_posterior(SEXP xp);
RcppExport SEXP set_PHI_posterior(SEXP xp, SEXP PHI_);
RcppExport SEXP set_P_posterior(SEXP xp, SEXP P_);

RcppExport SEXP get_ll_phi_components_posterior(SEXP xp);
RcppExport SEXP get_ll_p_components_posterior(SEXP xp);
RcppExport SEXP get_some_ll_phi_components_posterior(SEXP xp, SEXP indices);
RcppExport SEXP get_some_ll_p_components_posterior(SEXP xp, SEXP indices);
RcppExport SEXP get_log_posterior_posterior(SEXP xp);

#endif
