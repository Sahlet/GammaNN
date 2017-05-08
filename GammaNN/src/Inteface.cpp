// [[Rcpp::plugins(cpp11)]]

#include <Rcpp.h>
#include <My/Perceptron.h>
#include <assert.h>
#include <fstream>
#include <iostream>
#include <ctime>

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
Rcpp::List rcpp_hello() {

  My::Perceptron p(3);

  auto x = Rcpp::CharacterVector::create("foo", "bar");
  auto y = Rcpp::NumericVector::create(0.0, 1);
  auto z = Rcpp::List::create(x, y);


  return z;
}

void XOR_test(
    My::Perceptron& p,
    std::vector< My::Perceptron::pattern >& patterns
    ) {
  p = My::Perceptron(2, 1, { 2 });
  patterns = std::vector< My::Perceptron::pattern >{
    {
      { 0, 0 },{ 0 }
    }
    ,
    {
      { 0.1, 0.1 },{ 0 }
    }
    ,
    {
      { 0, 1 },{ 1 }
    }
    ,
    {
      { 0.1, 0.9 },{ 1 }
    }
    ,
    {
      { 1, 0 },{ 1 }
    }
    ,
    {
      { 0.9, 0.1 },{ 1 }
    }
    ,
    {
      { 1, 1 },{ 0 }
    }
    ,
    {
      { 0.9, 0.9 },{ 0 }
    }
  };
}

void linear_test(
    My::Perceptron& p,
    std::vector< My::Perceptron::pattern >& patterns
) {
  p = My::Perceptron(2, 1);
  patterns = std::vector< My::Perceptron::pattern > {
    {
      { 0, 0 }, { 0 }
    }
    ,
    {
      { 1, 0 },{ 1 }
    }
  };
}

// [[Rcpp::export]]
Rcpp::RObject test() {

  //auto seed = std::time(nullptr) % 1000;
  //std::cout << seed << std::endl;
  //srand(seed);

  //XOR test
  My::Perceptron p;
  std::vector< My::Perceptron::pattern > patterns;

  XOR_test(p, patterns);
  //linear_test(p, patterns);

  const double eps = 0.01;
  int epochs = 0;
  double err;

  do {
    err = std::pow(p.back_prop(patterns)*2, 0.5);
    epochs++;
    if (!(epochs%10000)) std::cout << epochs << " : err = " << err << std::endl;
  } while (err > eps);

  std::cout << "epochs = " << epochs << std::endl;

  //serializing test
  if (false)
  {
    std::stringstream s;
    (std::ostream&)s & p;

    auto str = s.str();

    p = My::Perceptron();
    (std::istream&)s & p;

    s = std::stringstream();
    (std::ostream&)s & p;

    std::cout << "\nserializing test " << (str == s.str() ? "passed" : "failed") << "\n\n";
  }

  {
    auto& weights = *(std::vector< My::matrix< double > >*)(&p);

    for(const auto& weight : weights) {
      std::cout << weight.to_string() << std::endl;
    }
  }

  for (auto& pattern : patterns) {
    assert(std::abs(p(pattern.input)[0] - pattern.second[0]) <= eps);
    std::cout << "[" << pattern.input[0] << ", " << pattern.input[1] << "] -> " << p(pattern.input)[0] << std::endl;
  }

  std::cout << std::flush;

  return Rcpp::RObject();
}
