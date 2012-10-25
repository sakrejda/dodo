#ifndef INTERFACE_H
#define INTERFACE_H

#include <Rcpp.h>
#include <survival.hpp>

RcppExport SEXP load_recapture_data(SEXP x);
RcppExport SEXP load_recapture_data(SEXP x);
RcppExport SEXP get_N(SEXP xp);
RcppExport SEXP get_K(SEXP xp);
RcppExport SEXP get_recaptures(SEXP xp, SEXP id);
RcppExport SEXP get_surveys(SEXP xp);

#endif
