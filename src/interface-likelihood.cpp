#include "interface-likelihood.hpp"
#include <survival_FLAT.hpp>
#include <RcppArmadillo.h>


RcppExport SEXP load_recapture_likelihood(SEXP x) {
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


	Recapture_Likelihood_FLAT* likelihood_ptr = new Recapture_Likelihood_FLAT(tos, tor, tods, kds);
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr( likelihood_ptr, true );
	return R_likelihood_ptr;
	END_RCPP
}

RcppExport SEXP get_N_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	int N;
	N = R_likelihood_ptr->get_N();
	return Rcpp::wrap(N);
	END_RCPP
}

RcppExport SEXP get_K_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	int K;
	K = R_likelihood_ptr->get_K();
	return Rcpp::wrap(K);
	END_RCPP
}


RcppExport SEXP get_recaptures_likelihood(SEXP xp, SEXP id) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	int i = Rcpp::as<int>(id); 	
	arma::Row<int> recaptures(R_likelihood_ptr->get_K());
	recaptures = R_likelihood_ptr->get_recaptures(i);
	return Rcpp::wrap(recaptures);
	END_RCPP
}


RcppExport SEXP get_surveys_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Row<int> surveys;
	surveys = R_likelihood_ptr->get_surveys();
	return Rcpp::wrap(surveys);
	END_RCPP
}

RcppExport SEXP get_births_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Row<int> births;
	births = R_likelihood_ptr->get_births();
	return Rcpp::wrap(births);
	END_RCPP
}


RcppExport SEXP get_first_obs_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Row<int> first_obs;
	first_obs = R_likelihood_ptr->get_first_obs();
	return Rcpp::wrap(first_obs);
	END_RCPP
}

RcppExport SEXP get_last_obs_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Row<int> last_obs;
	last_obs = R_likelihood_ptr->get_last_obs();
	return Rcpp::wrap(last_obs);
	END_RCPP
}

RcppExport SEXP get_sampled_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	std::vector<bool> sampled;
	sampled = R_likelihood_ptr->get_sampled();
	return Rcpp::wrap(sampled);
	END_RCPP
}


RcppExport SEXP get_deaths_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Row<int> deaths;
	deaths = R_likelihood_ptr->get_deaths();
	return Rcpp::wrap(deaths);
	END_RCPP
}

RcppExport SEXP set_deaths_likelihood(SEXP xp, SEXP id, SEXP td) {
	BEGIN_RCPP
	arma::Row<int> deaths;
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Col<arma::uword> i = Rcpp::as<arma::Col<arma::uword> >(id); 	
	arma::Col<int> times_of_deaths = Rcpp::as<arma::Col<int> >(td);
	R_likelihood_ptr->set_td(i, times_of_deaths);
	deaths = R_likelihood_ptr->get_deaths();
	return Rcpp::wrap(deaths);
	END_RCPP
}




RcppExport SEXP get_PHI_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Mat<double> PHI;
	PHI = R_likelihood_ptr->get_PHI();
	return Rcpp::wrap(PHI);
	END_RCPP
}

RcppExport SEXP get_P_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Mat<double> P;
	P = R_likelihood_ptr->get_P();
	return Rcpp::wrap(P);
	END_RCPP
}

RcppExport SEXP set_PHI_likelihood(SEXP xp, SEXP PHI_) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Mat<double> PHI = Rcpp::as<arma::Mat<double> >(PHI_);
	R_likelihood_ptr->set_PHI(PHI);
	PHI = R_likelihood_ptr->get_PHI();
	return Rcpp::wrap(PHI);
	END_RCPP
}

RcppExport SEXP set_P_likelihood(SEXP xp, SEXP P_) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Mat<double> P = Rcpp::as<arma::Mat<double> >(P_);
	R_likelihood_ptr->set_P(P);
	P = R_likelihood_ptr->get_P();
	return Rcpp::wrap(P);
	END_RCPP
}


RcppExport SEXP get_ll_phi_components_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Row<double> ll_phi_components;
	ll_phi_components = R_likelihood_ptr->get_ll_phi_components();
	return Rcpp::wrap(ll_phi_components);
	END_RCPP
}

RcppExport SEXP get_ll_p_components_likelihood(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Row<double> ll_p_components;
	ll_p_components = R_likelihood_ptr->get_ll_p_components();
	return Rcpp::wrap(ll_p_components);
	END_RCPP
}

RcppExport SEXP get_some_ll_phi_components_likelihood(SEXP xp, SEXP indices) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Row<double> ll_phi_components;
	arma::Col<arma::uword> indexes = Rcpp::as<arma::Col<arma::uword> >(indices);
	ll_phi_components = R_likelihood_ptr->get_ll_phi_components(indexes);
	return Rcpp::wrap(ll_phi_components);
	END_RCPP
}

RcppExport SEXP get_some_ll_p_components_likelihood(SEXP xp, SEXP indices) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood_FLAT> R_likelihood_ptr(xp);
	arma::Row<double> ll_p_components;
	arma::Col<arma::uword> indexes = Rcpp::as<arma::Col<arma::uword> >(indices);
	ll_p_components = R_likelihood_ptr->get_ll_p_components(indexes);
	return Rcpp::wrap(ll_p_components);
	END_RCPP
}
