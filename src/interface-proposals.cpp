#include "interface-proposals.hpp"
#include <proposals_FLAT.hpp>
#include <RcppArmadillo.h>


RcppExport SEXP load_recapture_posterior_proposal(SEXP x) {
	BEGIN_RCPP
	Rcpp::List rparam(x);
	std::vector<int> tos = Rcpp::as<std::vector<int> >(rparam["times_of_surveys"]);
	std::vector<std::vector<int> > tor;

	Rcpp::List r_tor = rparam["times_of_recaptures"];
	for (Rcpp::List::iterator i=r_tor.begin();
		i != r_tor.end(); ++i) {
		tor.push_back(Rcpp::as<std::vector<int> >(*i));
  }
	std::vector<int> tods = Rcpp::as<std::vector<int> >(rparam["times_of_deaths"]);
	std::vector<bool> kds = Rcpp::as<std::vector<bool> >(rparam["known_deaths"]);


	Recapture_Posterior_FLAT* posterior_ptr = new Recapture_Posterior_FLAT(tos, tor, tods, kds);
	Rcpp::XPtr<Recapture_Posterior_FLAT> R_posterior_ptr( posterior_ptr, true );

	Slice_td_Proposal_FLAT* slice_ptr = new Slice_td_Proposal_FLAT(*posterior_ptr);
	Rcpp::XPtr<Slice_td_Proposal_FLAT> R_slice_ptr( slice_ptr, true);

	return Rcpp::List::create(Rcpp::Named("posterior_ptr") = R_posterior_ptr,
														Rcpp::Named("slice_ptr")     = R_slice_ptr);
	END_RCPP
}

RcppExport SEXP propose_deaths_posterior_proposal(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Slice_td_Proposal_FLAT> R_slice_ptr(xp);
	arma::Row<int> new_deaths = R_slice_ptr->propose_td();
	return Rcpp::wrap(new_deaths);
	END_RCPP
}
