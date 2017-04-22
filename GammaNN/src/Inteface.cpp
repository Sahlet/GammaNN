// [[Rcpp::plugins(cpp11)]]

#include <Rcpp.h>
//#include "My/Perceptron.cpp"

// [[Rcpp::export]]
Rcpp::List rcpp_hello() {

  //My::Perceptron p(3);

  auto x = Rcpp::CharacterVector::create("foo", "bar");
  auto y = Rcpp::NumericVector::create(0.0, my_func());
  auto z = Rcpp::List::create(x, y);
  return z;
}
