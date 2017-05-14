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

    // patterns = std::vector< My::Perceptron::pattern >{
  //   {
  //     { 0, 0 },{ 0 }
  //   }
  //   ,
  //   {
  //     { 0.1, 0.1 },{ 0 }
  //   }
  //   ,
  //   {
  //     { 0, 1 },{ 1 }
  //   }
  //   ,
  //   {
  //     { 0.1, 0.9 },{ 1 }
  //   }
  //   ,
  //   {
  //     { 1, 0 },{ 1 }
  //   }
  //   ,
  //   {
  //     { 0.9, 0.1 },{ 1 }
  //   }
  //   ,
  //   {
  //     { 1, 1 },{ 0 }
  //   }
  //   ,
  //   {
  //     { 0.9, 0.9 },{ 0 }
  //   }
  // };

  patterns = std::vector< My::Perceptron::pattern > {
    {
      { 1, 1 },{ 1 }
    }
    ,
    {
      { 1, 2 },{ 2 }
    }
    ,
    {
      { 2, 1 },{ 2 }
    }
    ,
    {
      { 2, 2 },{ 1 }
    }
  };

  // patterns = std::vector< My::Perceptron::pattern >{
  //   {
  //     { -1, -1 },{ 0 }
  //   }
  //   ,
  //   {
  //     { -1, 1 },{ 1 }
  //   }
  //   ,
  //   {
  //     { 1, -1 },{ 1 }
  //   }
  //   ,
  //   {
  //     { 1, 1 },{ 0 }
  //   }
  // };
}

void linear_test(
    My::Perceptron& p,
    std::vector< My::Perceptron::pattern >& patterns
) {
  p = My::Perceptron(2, 1);
  patterns = std::vector< My::Perceptron::pattern > {
    {
      { 1, 1 },{ 0 }
    }
    ,
    {
      { 2, 1 },{ 1 }
    }
  };
}

void print_weights(const My::Perceptron& p) {
  auto& weights = *(const std::vector< My::matrix< double > >*)(&p);

  for(const auto& weight : weights) {
    std::cout << weight.to_string() << std::endl;
  }
}

// [[Rcpp::export]]
Rcpp::RObject test() {
  //auto seed = std::time(nullptr) % 1000;
  //std::cout << seed << std::endl;
  //srand(seed);

  My::Perceptron p;

  std::vector< My::Perceptron::pattern > patterns;

  XOR_test(p, patterns);
  //linear_test(p, patterns);

  const double eps = 0.001;
  int epochs = 0;
  double err;

  print_weights(p);

  do {
    err = std::pow(p.back_prop(patterns)*2, 0.5);
    epochs++;
    if (!(epochs%10000) || epochs == 1) std::cout << epochs << " : err = " << err << std::endl;
  } while (err > eps && epochs < 1000000);

  std::cout << "epochs = " << epochs << std::endl << "err = " << err << std::endl;

  //serializing test
  if (false)
  {
    std::stringstream s;
    s << p;

    auto str = s.str();

    p = My::Perceptron();
    s >> p;

    s = std::stringstream();
    s << p;

    std::cout << "\nserializing test " << (str == s.str() ? "passed" : "failed") << "\n\n";
  }

  print_weights(p);

  for (auto& pattern : patterns) {
    assert(std::abs(p(pattern.input)[0] - pattern.second[0]) <= eps);
    std::cout << "[" << pattern.input[0] << ", " << pattern.input[1] << "] -> " << p(pattern.input)[0] << std::endl;
  }

  std::cout << std::flush;

  return Rcpp::RObject();
}
