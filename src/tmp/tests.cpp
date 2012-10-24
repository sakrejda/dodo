#include "tests.hpp"

SEXP load_recapture_data(SEXP x) {
	Normal_Posterior parameters (x);
	double o = parameters.get_lpd();
	return Rcpp::wrap(o);
}

SEXP normal_chain(SEXP x, SEXP n, SEXP o) {
	int nx = Rcpp::as<int>(n);
	std::string out_file = Rcpp::as<std::string>(o);
	Chain<Normal_Posterior, Normal_Proposal> chain (x);
	chain.run(nx, out_file);
	return Rcpp::wrap(0);
}

SEXP student_lpd(SEXP x) {
	Student_Posterior parameters (x);
	double o = parameters.get_lpd();
	return Rcpp::wrap(o);
}


SEXP student_chain(SEXP x, SEXP n, SEXP o) {
	int nx = Rcpp::as<int>(n);
	std::string out_file = Rcpp::as<std::string>(o);
	Chain<Student_Posterior, Student_Proposal> chain (x);
	chain.run(nx, out_file);
	return Rcpp::wrap(0);
}

SEXP cjs_lpd(SEXP x) {
	CJS_Posterior parameters (x);
	double o = parameters.get_lpd();
	return Rcpp::wrap(o);
}

SEXP cjs_chain(SEXP x, SEXP n, SEXP o) {
	int nx = Rcpp::as<int>(n);
	std::string out_file = Rcpp::as<std::string>(o);
	Chain<CJS_Posterior, CJS_Proposal> chain (x);
	chain.run(nx, out_file);
	return Rcpp::wrap(0);
}

SEXP cjs_ram_chain(SEXP x, SEXP n, SEXP o) {
	int nx = Rcpp::as<int>(n);
	std::string out_file = Rcpp::as<std::string>(o);
	Chain<CJS_Posterior, CJS_RAM_Proposal> chain (x);
	chain.run(nx, out_file);
	return Rcpp::wrap(0);
}

SEXP multivariate_rnorm(SEXP n_in, SEXP mu_in, SEXP Sigma_in) {
	unsigned int n = Rcpp::as<int>(n_in);
	arma::Col<double> mu = Rcpp::as<arma::Col<double> >(mu_in);
	Rcpp::NumericMatrix r_Sigma(Sigma_in);
	int l = r_Sigma.nrow(), k = r_Sigma.ncol();
	arma::Mat<double> Sigma(r_Sigma.begin(), l, k, false); 
	arma::Mat<double> out;
	out = mvrnorm(n, mu, Sigma);
	return Rcpp::wrap(out);
}


RcppExport SEXP dgeom(SEXP x, SEXP p, SEXP return_log) {
	double xx = Rcpp::as<int>(x);
	double pp = Rcpp::as<double>(p);
	bool rl = Rcpp::as<bool>(return_log);
	double out = local_dgeom(xx, pp, rl);
	return Rcpp::wrap(out);
}
