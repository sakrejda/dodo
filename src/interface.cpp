#include "interface.hpp"
#include <survival.hpp>


RcppExport SEXP load_recapture_data(SEXP x) {
	Rcpp::List rparam(x);
	std::vector<int> tos = Rcpp::as<std::vector<int> >(rparam["times_of_surveys"]);
	std::vector<std::vector<int> > tor;

//  Rcpp::List r_obs_alive;
//  r_obs_alive = rparam["obs_alive"];
	Rcpp::List r_tor = rparam["times_of_recaptures"];
//  for (Rcpp::List::iterator i=r_obs_alive.begin();
	for (Rcpp::List::iterator i=r_tor.begin();
//      i != r_obs_alive.end(); ++i) {
		i != r_tor.end(); ++i) {
//    xx = Rcpp::as<std::vector<int> >(*i);
//    obs_alive.push_back(xx);
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


