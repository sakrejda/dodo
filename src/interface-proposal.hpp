#ifndef INTERFACE_PROPOSAL_H
#define INTERFACE_PROPOSAL_H

#include <Rcpp.h>
#include <survival.hpp>

RcppExport SEXP load_recapture_proposal(SEXP x);
RcppExport SEXP get_N_proposal(SEXP xp);
RcppExport SEXP get_K_proposal(SEXP xp);
RcppExport SEXP get_recaptures_proposal(SEXP xp, SEXP id);
RcppExport SEXP get_surveys_proposal(SEXP xp);
RcppExport SEXP get_births_proposal(SEXP xp);
RcppExport SEXP get_first_obs_proposal(SEXP xp);
RcppExport SEXP get_last_obs_proposal(SEXP xp);
RcppExport SEXP get_sampled_proposal(SEXP xp);
RcppExport SEXP get_deaths_proposal(SEXP xp);
RcppExport SEXP set_deaths_proposal(SEXP xp, SEXP id, SEXP td);

RcppExport SEXP get_PHI_proposal(SEXP xp);
RcppExport SEXP get_P_proposal(SEXP xp);
RcppExport SEXP set_PHI_proposal(SEXP xp, SEXP PHI_);
RcppExport SEXP set_P_proposal(SEXP xp, SEXP P_);

RcppExport SEXP get_ll_phi_components_proposal(SEXP xp);
RcppExport SEXP get_ll_p_components_proposal(SEXP xp);
RcppExport SEXP get_some_ll_phi_components_proposal(SEXP xp, SEXP indices);
RcppExport SEXP get_some_ll_p_components_proposal(SEXP xp, SEXP indices);
RcppExport SEXP get_log_posterior_proposal(SEXP xp);

RcppExport SEXP new_deaths_proposal(SEXP xp);


#endif
