// Generated by My::USing Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>
#include <iostream>
#include <My/GammaNN.h>

using namespace Rcpp;

typedef Rcpp::XPtr< My::GammaNN > NNptr;

// rcpp_hello
Rcpp::List rcpp_hello();
RcppExport SEXP GammaNN_rcpp_hello() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello());
    return rcpp_result_gen;
END_RCPP
}

// test
Rcpp::RObject test();
RcppExport SEXP GammaNN_test() {
  BEGIN_RCPP
  return Rcpp::wrap(test());
  END_RCPP
}

// learn
NNptr learn(
      Rcpp::DataFrame frame, std::vector< My::US > hidden, My::US units, My::US trace_size,
      const double eps, My::UI batch_size, const bool random_patterns, const My::UI max_epoch_number
    );
RcppExport SEXP GammaNN_learn(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6, SEXP arg7, SEXP arg8) {
  BEGIN_RCPP
  return Rcpp::wrap(
    learn(
      Rcpp::DataFrame(arg1), Rcpp::as< std::vector<My::US> >(arg2),
      Rcpp::as< My::US >(arg3), Rcpp::as< My::US >(arg4),
      Rcpp::as< double >(arg5), Rcpp::as< My::UI >(arg6), Rcpp::as< bool >(arg7),
      Rcpp::as< My::UI >(arg8)
    )
  );
  END_RCPP
}

//get_series
Rcpp::DataFrame get_series(NNptr R_NN, std::vector< My::US > object_numbers);
RcppExport SEXP GammaNN_get_series(SEXP arg1, SEXP arg2) {
  BEGIN_RCPP
  auto object_numbers = Rcpp::as< std::vector<My::US> >(arg2);
  for (auto& number : object_numbers) {
    if (number) number--;
  }
  return Rcpp::wrap(
    get_series(Rcpp::as< NNptr >(arg1), std::move(object_numbers))
  );
  END_RCPP
}

//get_col_names
std::vector< std::string > get_col_names(NNptr R_NN);
RcppExport SEXP GammaNN_get_col_names(SEXP arg1) {
  BEGIN_RCPP
  return Rcpp::wrap(
    get_col_names(Rcpp::as< NNptr >(arg1))
  );
  END_RCPP
}

//get_series_length
My::UI get_series_length(NNptr R_NN);
RcppExport SEXP GammaNN_get_series_length(SEXP arg1) {
  BEGIN_RCPP
  return Rcpp::wrap(
    get_series_length(Rcpp::as< NNptr >(arg1))
  );
  END_RCPP
}

//get_src_series_length
My::UI get_src_series_length(NNptr R_NN);
RcppExport SEXP GammaNN_get_src_series_length(SEXP arg1) {
  BEGIN_RCPP
  return Rcpp::wrap(
    get_src_series_length(Rcpp::as< NNptr >(arg1))
  );
  END_RCPP
}

//to_GammaNN
NNptr to_GammaNN(std::string str);
RcppExport SEXP GammaNN_to_GammaNN(SEXP arg1) {
  BEGIN_RCPP
  return Rcpp::wrap(
    to_GammaNN(Rcpp::as< std::string >(arg1))
  );
  END_RCPP
}

//to_str
std::string to_str(NNptr R_NN);
RcppExport SEXP GammaNN_to_str(SEXP arg1) {
  BEGIN_RCPP
  return Rcpp::wrap(
    to_str(Rcpp::as< NNptr >(arg1))
  );
  END_RCPP
}

//create_from_file
NNptr create_from_file(std::string file_path);
RcppExport SEXP GammaNN_create_from_file(SEXP arg1) {
  BEGIN_RCPP
  return Rcpp::wrap(
    create_from_file(Rcpp::as< std::string >(arg1))
  );
  END_RCPP
}

//write_to_file
void write_to_file(NNptr R_NN, std::string file_path);
RcppExport SEXP GammaNN_write_to_file(SEXP arg1, SEXP arg2) {
  BEGIN_RCPP
  write_to_file(Rcpp::as< NNptr >(arg1), Rcpp::as< std::string >(arg2));
  return Rcpp::wrap(Rcpp::RObject());
  END_RCPP
}
