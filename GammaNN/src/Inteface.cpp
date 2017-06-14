// [[Rcpp::plugins(cpp11)]]

#include <Rcpp.h>
//#include <XPtr.h>
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

void GammaNN_test(
    const My::matrix< double >& series,
    std::vector< My::US > hidden = {3, 3}, My::US units = 1, My::US trace_size = 6,
    const double eps = 0.01, My::UI batch_size = 1, const bool random_patterns = false,
    const My::UI max_epoch_number = 100000
) {

  if (series.width() < 1 || series.height() < 2) throw std::invalid_argument("series.width() < 1 || series.height() < 2");

  auto src_data = sub_matrix(series, {0, 0}, series.width(), series.height() / 2);

  My::GammaNN NN(src_data, hidden, units, trace_size);

  std::vector< My::UI > patterns(src_data.height() - NN.get_min_learn_pattern());

  for(int i = 0; i < patterns.size(); i++) {
    patterns[i] = i + NN.get_min_learn_pattern();
  }

  int epochs = 0;
  double err;

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
  } while (err > eps && epochs < max_epoch_number);

  NN.clear_learning();

  //if (err > eps)
  std::cout << "epochs = " << epochs << std::endl << "err = " << err << std::endl;

  //serializing test
  //if (false)
  {
    std::stringstream s;
    s << NN;

    auto str = s.str();

    NN = My::GammaNN();
    s >> NN;

    s = std::stringstream();
    s << NN;

    std::cout << "\nserializing test " << (str == s.str() ? "passed" : "failed") << "\n\n";
  }

  int i_restrictor = series.height()/2 + std::min(20, series.height() / 2);
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

  My::matrix< double > get_periodical_series1() {
    int periods = 6;
    std::vector< double > values = {
      0,
      0.03587,
      0.05167,
      0,
      0,
      0.05028,
      0.02931
    };
    std::vector< double > vec(periods * values.size());
    for (int i = 0; i < periods; i++) {
      for (int j = 0; j < values.size(); j++) {
        vec[i*values.size() + j] = values[j];
      }
    }
    return My::matrix< double >(1, vec.size(), std::move(vec));
  }
}

// [[Rcpp::export]]
Rcpp::RObject test() {
  GammaNN_test(get_periodical_series1());
  //perceptron_test();
  return Rcpp::RObject();
}

typedef Rcpp::XPtr< My::GammaNN > NNptr;

// [[Rcpp::export]]
NNptr learn(  Rcpp::DataFrame frame, std::vector< My::US > hidden, My::US units, My::US trace_size,
              const double eps, My::UI batch_size, const bool random_patterns, const My::UI max_epoch_number
            ) {

  std::srand(1);

  My::matrix<double> src_data(frame.ncol(), frame.nrow());

  std::vector< Rcpp::NumericVector > column(frame.ncol());

  for (My::US j = 0; j < frame.ncol(); j++) {
    column[j] = frame[j];
  }

  for (My::US i = 0; i < src_data.height(); i++) {
    for (My::US j = 0; j < src_data.width(); j++) {
      src_data(i, j) = column[j][i];
    }
  }

  column.clear();

  NNptr R_NN(new My::GammaNN(std::move(src_data), hidden, units, trace_size));

  My::GammaNN &NN = *R_NN;

  std::vector< My::UI > patterns(src_data.height() - NN.get_min_learn_pattern());

  if (batch_size > patterns.size()) batch_size = patterns.size();

  for(int i = 0; i < patterns.size(); i++) {
    patterns[i] = i + NN.get_min_learn_pattern();
  }

  int epochs = 0;
  double err;

  if (max_epoch_number) do {
    err = 0;
    if (random_patterns) {
      int iter_count = patterns.size() / batch_size + ((patterns.size() % batch_size) > 0);
      for (int i = 0; i < iter_count; i++) {
        err += std::pow(NN.learn(get_random_subvector(patterns, batch_size))*2, 0.5);
      }
    } else {
      auto iter = patterns.begin();
      while (true) {
        err += std::pow(NN.learn(std::vector< My::UI >(iter, iter + std::min((int)batch_size, (int)(patterns.end() - iter))))*2, 0.5);
        if (iter >= patterns.end() - batch_size) break;
        iter += batch_size;
      }
    }
    epochs++;
    if (!(epochs%1000) || epochs == 1) std::cout << epochs << " : err = " << err << std::endl;
  } while (err > eps && epochs < max_epoch_number);

  NN.clear_learning();

  //if (err > eps)
  std::cout << "epochs = " << epochs << std::endl << "err = " << err << std::endl;

  NN.set_col_names(Rcpp::as< std::vector< std::string > > (frame.names()));

  //print_weights(NN.get_percreptron());

  return R_NN;
}

// [[Rcpp::export]]
Rcpp::DataFrame get_series(NNptr R_NN, std::vector< My::US > object_numbers) {
  Rcpp::DataFrame frame;

  if (!object_numbers.size()) return frame;

  std::vector< std::vector< double > > columns(R_NN->get_object_dimention());
  for (auto& column : columns) {
    column.resize(object_numbers.size());
  }

  for (int j = 0; j < object_numbers.size(); j++) {
    auto object = (*R_NN)[object_numbers[j]];
    for (int i = 0; i < columns.size(); i++) {
      columns[i][j] = object[i];
    }
  }

  auto col_names = R_NN->get_col_names();
  for (int i = 0; i < columns.size(); i++) {
    frame.push_back(std::move(columns[i]), col_names[i]);
  }

  return frame;
}

// [[Rcpp::export]]
std::vector< std::string > get_col_names(NNptr R_NN) {
  return R_NN->get_col_names();
}

// [[Rcpp::export]]
My::UI get_series_length(NNptr R_NN) {
  return R_NN->get_series_size();
}

// [[Rcpp::export]]
My::UI get_src_series_length(NNptr R_NN) {
  return R_NN->get_src_series_size();
}

// [[Rcpp::export]]
NNptr to_GammaNN(std::string str) {
  NNptr R_NN(new My::GammaNN);
  std::stringstream s(str);
  s >> *R_NN;
  return R_NN;
}

// [[Rcpp::export]]
std::string to_str(NNptr R_NN) {
  std::stringstream s;
  s << *R_NN;
  return s.str();
}

// [[Rcpp::export]]
NNptr create_from_file(std::string file_path) {
  std::ifstream f(file_path);
  NNptr R_NN(new My::GammaNN(My::GammaNN::from_stream(f)));
  return R_NN;
}

// [[Rcpp::export]]
void write_to_file(NNptr R_NN, std::string file_path) {
  std::ofstream f(file_path);
  R_NN->write_to_stream(f);
}

Rcpp::DataFrame to_data_frame(const My::matrix< double >& m) {
  Rcpp::DataFrame frame;

  if (!m.width()) return frame;

  std::vector< std::vector< double > > columns(m.width());
  for (auto& column : columns) {
    column.resize(m.height());
  }

  for (int j = 0; j < m.height(); j++) {
    for (int i = 0; i < columns.size(); i++) {
      columns[i][j] = m(j, i);
    }
  }

  for (int i = 0; i < columns.size(); i++) {
    frame.push_back(std::move(columns[i]), "V"+std::to_string(i));
  }

  return frame;
}

My::matrix< double > to_matrix(const Rcpp::DataFrame& frame) {
  My::matrix< double > m(frame.ncol(), frame.nrow());

  std::vector< Rcpp::NumericVector > columns(frame.ncol());

  {
    int i = 0;
    for(auto& column : columns) {
      column = frame[i++];
    }
  }

  for (int j = 0; j < m.height(); j++) {
    for (int i = 0; i < m.width(); i++) {
      m(j, i) = columns[i][j];
    }
  }

  return std::move(m);
}

// [[Rcpp::export]]
Rcpp::DataFrame first_step_prediction_test(NNptr R_NN, const Rcpp::DataFrame& future_src_frame) {
  //future_src - frame that containes several future points

  //this function returns Rcpp::DataFrame with first step predictions using future_src parameter

  // struct guard_t {
  //   guard_t() {
  //     std::cout << "start first_step_prediction_test" << std::endl;
  //   };
  //   ~guard_t() {
  //     std::cout << "end first_step_prediction_test" << std::endl;
  //   }
  // } guard;

  const auto future_src = to_matrix(future_src_frame);

  if (!R_NN) throw std::invalid_argument("R_NN is nullptr");
  if (future_src.width() != R_NN->get_object_dimention())
    throw std::invalid_argument("future_src.width() != R_NN->get_object_dimention()");

  My::matrix< double > m(future_src.width(), future_src.height() + 1);

  My::GammaNN NN(*R_NN);

  auto src_series_size = NN.get_src_series_size();

  m[0] = NN[src_series_size];
  for(int i = 0; i < future_src.height();) {
    NN.fix_object(i + src_series_size, future_src[i]);
    i++;
    m[i] = NN[i + src_series_size];
  }

  // std::cout << "future_src:\n" << future_src.to_string() << std::endl;
  // std::cout << "m:\n" << m.to_string() << std::endl;

  return to_data_frame(m);
}
