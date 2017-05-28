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

void plus_one(
    My::Perceptron& p,
    std::vector< My::Perceptron::pattern >& patterns
) {
  p = My::Perceptron(1, 1, {});
    patterns = std::vector< My::Perceptron::pattern > {
    {
      { 0 },{ 1 }
    }
    ,
    {
      { 1 },{ 2 }
    }
    // ,
    // {
    //   { 2 },{ 3 }
    // }
    // ,
    // {
    //   { 3 },{ 4 }
    // }
  };
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

void perceptron_test() {
  //auto seed = std::time(nullptr) % 1000;
  //std::cout << seed << std::endl;
  //srand(seed);

  My::Perceptron p;

  std::vector< My::Perceptron::pattern > patterns;

  //XOR_test(p, patterns);
  //linear_test(p, patterns);
  plus_one(p, patterns);

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
    std::cout << pattern.input << " -> " << p(pattern.input) << std::endl;
  }

  //std::cout << 6 << " -> " << p({6}) << std::endl;

  std::cout << std::flush;
}
}

namespace {


template <class T>
std::vector< T > get_random_subvector(const std::vector< T >& vec, My::US size = 1) {
  if (!vec.size() && size) throw std::invalid_argument("empty vec");
  std::vector< T > res;
  res.reserve(size);
  for (My::US i = 0; i < size; i++) {
    res.push_back(vec[std::rand()%vec.size()]);
  }

  return std::move(res);
}

void GammaNN_test(const My::matrix< double >& series) {

  if (series.width() < 1 || series.height() < 2) throw std::invalid_argument("series.width() < 1 || series.height() < 2");

  auto src_data = sub_matrix(series, {0, 0}, series.width(), series.height() / 2);

  My::GammaNN NN(src_data, { 2 }, 2, 0);

  std::vector< My::UI > patterns(src_data.height() - NN.get_min_learn_pattern());

  for(int i = 0; i < patterns.size(); i++) {
    patterns[i] = i + NN.get_min_learn_pattern();
  }

  const double eps = 0.01;
  int epochs = 0;
  double err;

  const bool random_patterns = false;
  const int batch_size = 1;//patterns.size();

  do {
    err = 0;
    if (random_patterns) {
      int iter_count = (1 * patterns.size()) / batch_size;
      for (int i = 0; i < iter_count; i++) {
        err += std::pow(NN.learn(get_random_subvector(patterns, batch_size))*2, 0.5);
      }
    } else {
      for (auto iter = patterns.begin() + batch_size; true; iter++) {
        err += std::pow(NN.learn(std::vector< My::UI >(iter - batch_size, iter))*2, 0.5);
        if (iter == patterns.end()) break;
      }
    }
    epochs++;
    if (!(epochs%10000) || epochs == 1) std::cout << epochs << " : err = " << err << std::endl;
  } while (err > eps && epochs < 100000);

  NN.clear_learning();

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

  My::matrix< double > get_series_of_one() {
    std::vector< double > vec(50, 1);
    return My::matrix< double >(1, vec.size(), std::move(vec));
  }
  My::matrix< double > get_series_zero_and_one() {
    std::vector< double > vec(10);
    for (int i = 0; i < vec.size(); i++) {
      vec[i] = i % 2;
    }
    return My::matrix< double >(1, vec.size(), std::move(vec));
  }
  My::matrix< double > get_growing_series() {
    std::vector< double > vec(6);
    for (int i = 0; i < vec.size(); i++) {
      vec[i] = i;
    }
    return My::matrix< double >(1, vec.size(), std::move(vec));
  }
}

// [[Rcpp::export]]
Rcpp::RObject test() {
  GammaNN_test(get_series_zero_and_one());
  //perceptron_test();
  return Rcpp::RObject();
}
