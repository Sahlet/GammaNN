// [[Rcpp::plugins(cpp11)]]

#include <Rcpp.h>
#include <My/Perceptron.h>
#include <My/GammaNN.h>
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

namespace {
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

void perceptron_test() {
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

  //if (err > eps)
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
}
}

namespace {
My::matrix< double > get_series_of_one() {
  std::vector< double > vec(100, 1);
  return My::matrix< double >(1, vec.size(), std::move(vec));
}

void GammaNN_test(const My::matrix< double >& series) {

  if (series.width() < 1 || series.height() < 2) throw std::invalid_argument("series.width() < 1 || series.height() < 2");

  auto src_data = sub_matrix(series, {0, 0}, series.width(), series.height() / 2);

  My::GammaNN NN(src_data, { }, 1, 1);

  std::vector< My::UI > patterns(src_data.height() - 1);

  for(int i = 0; i < patterns.size(); i++) {
    patterns[i] = i+1;
  }

  const double eps = 0.01;
  int epochs = 0;
  double err;

  do {
    err = std::pow(NN.learn(patterns)*2, 0.5);
    epochs++;
    if (!(epochs%10000) || epochs == 1) std::cout << epochs << " : err = " << err << std::endl;
  } while (err > eps && epochs < 100000);

  //if (err > eps)
  std::cout << "epochs = " << epochs << std::endl << "err = " << err << std::endl;

  ////serializing test
  // if (false)
  // {
  //   std::stringstream s;
  //   s << NN;
  //
  //   auto str = s.str();
  //
  //   p = My::Perceptron();
  //   s >> p;
  //
  //   s = std::stringstream();
  //   s << p;
  //
  //   std::cout << "\nserializing test " << (str == s.str() ? "passed" : "failed") << "\n\n";
  // }

  int i_restrictor = series.height()/2 + std::min(4, series.height() / 2);
  for (int i = series.height() / 2; i < i_restrictor; i++) {
    std::cout << i << " : "
      << "real = " << std::vector< double >(series[i])
      << " \t\t "
      << "NN = " << NN[i]
      << std::endl;
  }

  std::cout << std::flush;
}

}

// [[Rcpp::export]]
Rcpp::RObject test() {
  GammaNN_test(get_series_of_one());
  return Rcpp::RObject();
}
