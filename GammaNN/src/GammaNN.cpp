#include <My/GammaNN.h>

namespace My {

typedef unsigned int UI;

  class Series {
    const My::matrix< double > src_data;
    std::vector< GammaNN::object > gen_data;
  public:

    Series(My::matrix< double > src_data) : src_data(std::move(src_data)) {}

    US get_object_dimention() {
      return src_data.width();
    }

    inline UI series_size() {
      return src_data.height() + gen_data.size();
    }

    GammaNN::object operator[](int index) throw (std::out_of_range) {
      if (index >= series_size()) throw std::out_of_range("Series: out_of_range");

      if (index < src_data.height()) {
        auto iter = src_data.begin() + (index * src_data.width());
        return GammaNN::object(iter, iter + (src_data.width() - 1));
      }

      return gen_data[index - src_data.height()];
    }
  };

  class GammaUnit {
    Series& series;
    GammaNN::object weight;
    GammaNN::object weight_delta;
    GammaNN::object result;
    GammaNN::object result_by_derivative;
    UI result_number = 0;
  public:

    //            impulse response for Gamma memory "g(i, weight) = weight*((1 - weight)^(i - 1))" weight є ((0; 2) \ {1})
    //derivative  impulse response for Gamma memory "g'(i, weight) = (1 - i*weight)*((1 - weight)^(i - 2))" weight є ((0; 2) \ {1})

    //common
    //------------------------------------
    void clear_all() {
      clear_learning();
      set_value(weight, 0);
    }
    GammaUnit(Series& series) : series(series) {
      weight.resize(series.get_object_dimention());
      result.resize(series.get_object_dimention());
    }
    GammaNN::object get() {
      return weight * result;
    }


    //for prediction
    //------------------------------------
    void clear_result() {
      result_number = 0;
      set_value(result, 0);
    }
    void next() {
      if (result_number) result *= (1 - weight);
      result += series[result_number];
      result_number++;
    }


    //for learning
    //------------------------------------
    typedef Perceptron::errors errors;
    void clear_learning() {
      clear_result();
      set_value(result_by_derivative, 0);
      result_by_derivative.clear();
    }
    void random_weight_init();
    GammaNN::object operator[](const UI index) {
      clear_learning();

      GammaNN::object one_minus_weight = (1 - weight);

      result += series[0];
      for (result_number = 1; result_number <= index; result_number++) {
        result *= one_minus_weight;
        result += series[result_number];
      }

      result_by_derivative.resize(series.get_object_dimention());
      if (index) {
        result_by_derivative += series[index - 1];
        for (UI i = 2; i <= index; i++) {
          result_by_derivative += i * (series[index - i] * one_minus_weight);
          if (i < index) one_minus_weight *= one_minus_weight;
        }
      }
      result_by_derivative = result - weight * result_by_derivative;

      return weight * result;
    }
    GammaUnit::errors put_errors(Perceptron::errors e) throw (std::invalid_argument) {
      if (e.size() != series.get_object_dimention())
        throw std::invalid_argument("GammaUnit(): invalid errors dimention");

      weight_delta.resize(series.get_object_dimention());
      e *= result_by_derivative;
      const double speed = 1;
      weight_delta += speed * e;

      return std::move(e);
    }

    void flush() {
      weight -= weight_delta;
      weight_delta.clear();
    }

  };

  struct GammaNN_members {
    Series series;

    std::vector< GammaUnit > units;
    My::Perceptron p;

    GammaNN_members(My::matrix< double > src_data) : series(std::move(src_data)) {}
  };

  US GammaNN::get_object_dimention() {
    return members->series.get_object_dimention();
  }

  US GammaNN::get_units_number() {
    return members->units.size();
  }

  GammaNN::GammaNN(
    My::matrix< double > data,
    const std::vector< US >& hidden,
    US units
  ) throw (std::invalid_argument) {
    if (data.width() < 1 || data.height() < 1)
      throw std::invalid_argument("GammaNN(): invalid data dimention");

    members.reset(new GammaNN_members(std::move(data)));

    if (units) {
      members->units.reserve(units);
      for (US i = 0; i < units; i++) {
        members->units.emplace_back(members->series);
      }
    }

    members->p = My::Perceptron(get_object_dimention() * (get_units_number() + 1), get_object_dimention(), hidden);

    //learning
  }

  GammaNN::object GammaNN::operator[](int index) {
    GammaNN::object res;
  }
}
