#include "interface-data.hpp"
#include <survival.hpp>
#include <RcppArmadillo.h>


RcppExport SEXP load_recapture_data(SEXP x) {
	BEGIN_RCPP
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
	END_RCPP
}

RcppExport SEXP get_N_data(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	int N;
	N = R_data_ptr->get_N();
	return Rcpp::wrap(N);
	END_RCPP
}

RcppExport SEXP get_K_data(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	int K;
	K = R_data_ptr->get_K();
	return Rcpp::wrap(K);
	END_RCPP
}


RcppExport SEXP get_recaptures_data(SEXP xp, SEXP id) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	int i = Rcpp::as<int>(id); 	
	arma::Row<int> recaptures(R_data_ptr->get_K());
	recaptures = R_data_ptr->get_recaptures(i);
	return Rcpp::wrap(recaptures);
	END_RCPP
}


RcppExport SEXP get_surveys_data(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	arma::Row<int> surveys;
	surveys = R_data_ptr->get_surveys();
	return Rcpp::wrap(surveys);
	END_RCPP
}

RcppExport SEXP get_births_data(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	arma::Row<int> births;
	births = R_data_ptr->get_births();
	return Rcpp::wrap(births);
	END_RCPP
}


RcppExport SEXP get_first_obs_data(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	arma::Row<int> first_obs;
	first_obs = R_data_ptr->get_first_obs();
	return Rcpp::wrap(first_obs);
	END_RCPP
}

RcppExport SEXP get_last_obs_data(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	arma::Row<int> last_obs;
	last_obs = R_data_ptr->get_last_obs();
	return Rcpp::wrap(last_obs);
	END_RCPP
}

RcppExport SEXP get_sampled_data(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Data_FLAT> R_data_ptr(xp);
	std::vector<bool> sampled;
	sampled = R_data_ptr->get_sampled();
	return Rcpp::wrap(sampled);
	END_RCPP
}






