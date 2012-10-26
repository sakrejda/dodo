#include "interface-state.hpp"
#include <survival.hpp>
#include <RcppArmadillo.h>


RcppExport SEXP load_recapture_state(SEXP x) {
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


	Recapture_State_FLAT* state_ptr = new Recapture_State_FLAT(tos, tor, tods, kds);
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr( state_ptr, true );
	return R_state_ptr;
}

RcppExport SEXP get_N_state(SEXP xp) {
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	int N;
	N = R_state_ptr->get_N();
	return Rcpp::wrap(N);
}

RcppExport SEXP get_K_state(SEXP xp) {
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	int K;
	K = R_state_ptr->get_K();
	return Rcpp::wrap(K);
}


RcppExport SEXP get_recaptures_state(SEXP xp, SEXP id) {
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	int i = Rcpp::as<int>(id); 	
	arma::Row<int> recaptures(R_state_ptr->get_K());
	recaptures = R_state_ptr->get_recaptures(i);
	return Rcpp::wrap(recaptures);
}


RcppExport SEXP get_surveys_state(SEXP xp) {
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Row<int> surveys;
	surveys = R_state_ptr->get_surveys();
	return Rcpp::wrap(surveys);
}

RcppExport SEXP get_births_state(SEXP xp) {
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Row<int> births;
	births = R_state_ptr->get_births();
	return Rcpp::wrap(births);
}


RcppExport SEXP get_first_obs_state(SEXP xp) {
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Row<int> first_obs;
	first_obs = R_state_ptr->get_first_obs();
	return Rcpp::wrap(first_obs);
}

RcppExport SEXP get_last_obs_state(SEXP xp) {
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Row<int> last_obs;
	last_obs = R_state_ptr->get_last_obs();
	return Rcpp::wrap(last_obs);
}

RcppExport SEXP get_deaths_state(SEXP xp) {
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Row<int> deaths;
	deaths = R_state_ptr->get_deaths();
	return Rcpp::wrap(deaths);
}






