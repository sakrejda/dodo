#include "interface.hpp"
#include <survival.hpp>
#include <RcppArmadillo.h>


RcppExport SEXP load_recapture_data(SEXP x) {
	Rcpp::List rparam(x);
	std::vector<int> tos = Rcpp::as<std::vector<int> >(rparam["times_of_surveys"]);
	std::vector<std::vector<int> > tor;

	Rcpp::List r_tor = rparam["times_of_recaptures"];
	for (Rcpp::List::iterator i=r_tor.begin();
		i != r_tor.end(); ++i) {
		tor.push_back(Rcpp::as<std::vector<int> >(*i));
  }


	Recapture_Data_FLAT* data_ptr = new Recapture_Data_FLAT(tos, tor);
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr( data_ptr, true );
	return R_data_ptr;
}

RcppExport SEXP get_N(SEXP xp) {
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	int N;
	N = R_data_ptr->get_N();
	return Rcpp::wrap(N);
}

RcppExport SEXP get_K(SEXP xp) {
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	int K;
	K = R_data_ptr->get_K();
	return Rcpp::wrap(K);
}


RcppExport SEXP get_recaptures(SEXP xp, SEXP id) {
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	int i = Rcpp::as<int>(id); 	
	arma::Row<int> recaptures(R_data_ptr->get_K());
	recaptures = R_data_ptr->get_recaptures(i);
	return Rcpp::wrap(recaptures);
}


RcppExport SEXP get_surveys(SEXP xp) {
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	arma::Row<int> surveys;
	surveys = R_data_ptr->get_surveys();
	return Rcpp::wrap(surveys);
}

RcppExport SEXP get_births(SEXP xp) {
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	arma::Row<int> births;
	births = R_data_ptr->get_births();
	return Rcpp::wrap(births);
}


RcppExport SEXP get_first_obs(SEXP xp) {
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	arma::Row<int> first_obs;
	first_obs = R_data_ptr->get_first_obs();
	return Rcpp::wrap(first_obs);
}

RcppExport SEXP get_last_obs(SEXP xp) {
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	arma::Row<int> last_obs;
	last_obs = R_data_ptr->get_last_obs();
	return Rcpp::wrap(last_obs);
}








