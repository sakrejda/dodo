#include "interface-state.hpp"
#include <survival_FLAT.hpp>
#include <RcppArmadillo.h>


RcppExport SEXP load_recapture_state(SEXP x) {
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


	Recapture_State_FLAT* state_ptr = new Recapture_State_FLAT(tos, tor, tods, kds);
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr( state_ptr, true );
	return R_state_ptr;
	END_RCPP
}

RcppExport SEXP get_N_state(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	int N;
	N = R_state_ptr->get_N();
	return Rcpp::wrap(N);
	END_RCPP
}

RcppExport SEXP get_K_state(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	int K;
	K = R_state_ptr->get_K();
	return Rcpp::wrap(K);
	END_RCPP
}


RcppExport SEXP get_recaptures_state(SEXP xp, SEXP id) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	int i = Rcpp::as<int>(id); 	
	arma::Row<int> recaptures(R_state_ptr->get_K());
	recaptures = R_state_ptr->get_recaptures(i);
	return Rcpp::wrap(recaptures);
	END_RCPP
}


RcppExport SEXP get_surveys_state(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Row<int> surveys;
	surveys = R_state_ptr->get_surveys();
	return Rcpp::wrap(surveys);
	END_RCPP
}

RcppExport SEXP get_births_state(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Row<int> births;
	births = R_state_ptr->get_births();
	return Rcpp::wrap(births);
	END_RCPP
}


RcppExport SEXP get_first_obs_state(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Row<int> first_obs;
	first_obs = R_state_ptr->get_first_obs();
	return Rcpp::wrap(first_obs);
	END_RCPP
}

RcppExport SEXP get_last_obs_state(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Row<int> last_obs;
	last_obs = R_state_ptr->get_last_obs();
	return Rcpp::wrap(last_obs);
	END_RCPP
}

RcppExport SEXP get_sampled_state(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	std::vector<bool> sampled;
	sampled = R_state_ptr->get_sampled();
	return Rcpp::wrap(sampled);
	END_RCPP
}

RcppExport SEXP get_deaths_state(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Row<int> deaths;
	deaths = R_state_ptr->get_deaths();
	return Rcpp::wrap(deaths);
	END_RCPP
}

RcppExport SEXP set_deaths_state(SEXP xp, SEXP id, SEXP td) {
	BEGIN_RCPP
	arma::Row<int> deaths;
	Rcpp::XPtr<Recapture_State_FLAT> R_state_ptr(xp);
	arma::Col<arma::uword> i = Rcpp::as<arma::Col<arma::uword> >(id); 	
	arma::Col<int> times_of_deaths = Rcpp::as<arma::Col<int> >(td);
	R_state_ptr->set_td(i, times_of_deaths);
	deaths = R_state_ptr->get_deaths();
	return Rcpp::wrap(deaths);
	END_RCPP
}






