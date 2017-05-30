//GammaNN.cpp

#ifndef __GAMMANN_CPP__
#define __GAMMANN_CPP__

#include <iostream>
#include <My/GammaNN.h>

#define MIN_WEIGHT 0.01
#define MAX_WEIGHT 1.99
#define AREA_AROUND_ONE 0.05

namespace My {

  class Series {
    friend class GammaNN;
    const My::matrix< double > src_data;
    std::vector< GammaNN::object > gen_data;
  public:

    Series(My::matrix< double > src_data = My::matrix< double >()) : src_data(std::move(src_data)) {}

    US get_object_dimention() {
      return src_data.width();
    }

    inline UI size() {
      return src_data.height() + gen_data.size();
    }

    GammaNN::object operator[](int index) throw (std::out_of_range) {
      if (index >= size()) throw std::out_of_range("Series: out_of_range");

      if (index < src_data.height()) {
        auto iter = src_data.begin() + (index * src_data.width());
        return GammaNN::object(iter, iter + src_data.width());
      }

      return gen_data[index - src_data.height()];
    }
  };

  class GammaUnit {
    friend inline std::ostream& operator & (std::ostream& os, const My::GammaUnit& g);
    friend inline std::istream& operator & (std::istream& is, My::GammaUnit& g);
    GammaNN::object weight;
    GammaNN::object one_minus_weight;
    GammaNN::object weight_delta;
    GammaNN::object result;
    GammaNN::object result_by_derivative;
    UI result_number = 0;

#define SERIALIZE_GammaUnit(STREAM, GUNIT)\
          STREAM                          \
          & GUNIT.weight                  \
          & GUNIT.one_minus_weight        \
          & GUNIT.weight_delta            \
          & GUNIT.result                  \
          & GUNIT.result_by_derivative    \
          & GUNIT.result_number

  public:
    Series* series_ptr = nullptr;

    //            impulse response for Gamma memory "g(i, weight) = weight*((1 - weight)^(i - 1))" weight є ((0; 2) \ {1})
    //derivative  impulse response for Gamma memory "g'(i, weight) = (1 - i*weight)*((1 - weight)^(i - 2))" weight є ((0; 2) \ {1})

    //common
    //------------------------------------
    void clear_all() {
      clear_learning();
      set_value(weight, 0);
      set_value(one_minus_weight, 1);
    }

    GammaUnit() {}
    GammaUnit(Series& series) : series_ptr(&series) {
      weight.resize(series.get_object_dimention());
      one_minus_weight.resize(series.get_object_dimention());
      set_value(one_minus_weight, 1);
      result.resize(series.get_object_dimention());
      random_weight_init();
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
      if (result_number) result *= one_minus_weight;
      result += (*series_ptr)[result_number];
      result_number++;
    }


    //for learning
    //------------------------------------
    typedef Perceptron::errors errors;
    void clear_learning() {
      clear_result();
      result_by_derivative.clear();
      weight_delta.clear();
    }

    void clamp_weight() {
      weight = clamp(weight, MIN_WEIGHT, MAX_WEIGHT);
      for (auto& w : weight) {
        if (w > (1 - AREA_AROUND_ONE/2) && w < (1 + AREA_AROUND_ONE/2)) {
          w = (1 - AREA_AROUND_ONE/2) + (std::rand()%2)*AREA_AROUND_ONE;
        }
      }
      one_minus_weight = 1 - weight;
    }

    void random_weight_init() {
      for (auto& w : weight) {
        w = (MIN_WEIGHT + ((std::rand() % 10000) / 10000.0) * (MAX_WEIGHT - MIN_WEIGHT));
      }
      clamp_weight();
      //one_minus_weight = 1 - weight;
    }
    GammaNN::object operator[](const UI index) {
      clear_result();

      result += (*series_ptr)[0];
      for (result_number = 1; result_number <= index; result_number++) {
        result *= one_minus_weight;
        result += (*series_ptr)[result_number];
      }

      set_value(result_by_derivative, 0);
      result_by_derivative.resize((*series_ptr).get_object_dimention());
      {
        if (index) {
          result_by_derivative = (*series_ptr)[index - 1];
          auto one_minus_weight_copy = one_minus_weight;
          for (UI i = 2; i <= index; i++) {
            result_by_derivative += i * ((*series_ptr)[index - i] * one_minus_weight_copy);
            if (i < index) one_minus_weight_copy *= one_minus_weight_copy;
          }
        }
        result_by_derivative = result - weight * result_by_derivative;
      }

      return weight * result;
    }
    GammaUnit::errors put_errors(Perceptron::errors e) throw (std::invalid_argument) {
      if (e.size() != (*series_ptr).get_object_dimention())
        throw std::invalid_argument("GammaUnit(): invalid errors dimention");

      weight_delta.resize((*series_ptr).get_object_dimention());

      e *= result_by_derivative;

      weight_delta += e;

      //set_value(result_by_derivative, 0);

      return std::move(e);
    }

    //int counter = 1;

    void flush() {
      const double speed = 1;
      weight -= speed * weight_delta;
      clamp_weight();
      //one_minus_weight = 1 - weight;
      set_value(weight_delta, 0);

      ////if (counter == 1) std::cout << counter << " : weight = " << weight << std::endl;
      //if (!(counter%10000) || counter == 1) std::cout << counter << " : weight = " << weight << std::endl;
      //counter++;
    }

  };

  inline std::ostream& operator & (std::ostream& os, const My::GammaUnit& g) {
    SERIALIZE_GammaUnit(os, g);
    return os;
  }
  inline std::istream& operator & (std::istream& is, My::GammaUnit& g) {
    SERIALIZE_GammaUnit(is, g);
    return is;
  }

  struct GammaNN_members {
    Series series;
    std::vector< GammaUnit > units;
    const UI trace_size;
    My::Perceptron p;
    std::vector< std::string > col_names;

    GammaNN_members(My::matrix< double > src_data, const UI trace_size) : series(std::move(src_data)), trace_size(trace_size) {}
  };

  void GammaNN_members_deleter::operator()(GammaNN_members* ptr) {
    delete ptr;
  }

  US GammaNN::get_object_dimention() {
    return members->series.get_object_dimention();
  }
  US GammaNN::get_units_number() {
    return members->units.size();
  }
  US GammaNN::get_trace_size() {
    return members->trace_size;
  }
  US GammaNN::get_min_learn_pattern() {
    return std::max(get_trace_size(), US(1));
  }
  US GammaNN::get_series_size() {
    return members->series.size();
  }
  US GammaNN::get_src_series_size() {
    return members->series.src_data.height();
  }

  std::vector< std::string > GammaNN::get_col_names() {
    return members->col_names;
  }
  void GammaNN::set_col_names(std::vector< std::string > names) {
    if (names.size() != get_object_dimention()) throw (std::invalid_argument("incorect names vector dimention"));
    members->col_names = std::move(names);
  }


  GammaNN::GammaNN(
    My::matrix< double > data,
    const std::vector< US >& hidden,
    US units,
    US trace_size
  ) throw (std::invalid_argument) {
    if ((trace_size + units) == 0)
      throw std::invalid_argument("GammaNN(): (trace_size + units) == 0 -- perceptron can't see anything");

    if (data.width() < 1 || data.height() < trace_size)
      throw std::invalid_argument("GammaNN(): invalid data dimention");

    members.reset(new GammaNN_members(std::move(data), trace_size));

    if (units) {
      members->units.reserve(units);
      for (US i = 0; i < units; i++) {
        members->units.emplace_back(members->series);
      }
    }

    members->p = My::Perceptron(get_object_dimention() * (get_units_number() + get_trace_size()), get_object_dimention(), hidden);

    members->col_names.reserve(get_object_dimention());
    for (US i = 0; i < get_object_dimention(); i++) {
      members->col_names.emplace_back("x" + std::to_string(i));
    }

  }

  double GammaNN::learn(const std::vector< UI >& patterns) throw (std::range_error) {
    if (!patterns.size()) return 0;

    UI series_size = members->series.size();
    for (auto& pattern : patterns) {
      if (pattern >= series_size || pattern < get_min_learn_pattern())
        throw std::range_error("pattern >= series_size || pattern < get_min_learn_pattern()");
    }

    double global_error = 0;

    My::matrix< double > input(get_object_dimention(), get_units_number() + get_trace_size());
    for (auto& pattern : patterns) {
      for (UI j = 0; j < get_units_number(); j++) {
        input[j] = members->units[j][pattern - 1];
      }
      for (UI j = get_units_number(), obj_number = 1; j < input.height(); j++, obj_number++) {
        input[j] = members->series[pattern - obj_number];
      }

      //static int count = 0;
      //if (count++ < 20) std::cout << " input = " << input.get_vec() << std::endl;

      auto e = members->p.forward_prop(input.get_vec()) - members->series[pattern];
      global_error += (e, e);
      e = members->p.put_errors(std::move(e), false);

      My::matrix< double > errors (
        input.width(), input.height(),
        std::move(e)
      );

      for (UI j = 0; j < get_units_number(); j++) {
        members->units[j].put_errors(errors[j]);
      }
    }

    members->p.flush();
    for(auto& unit : members->units) {
      unit.flush();
    }

    return global_error / 2;
  }

  void GammaNN::clear_learning() {
    for(auto& unit : members->units) {
      unit.clear_learning();
    }
    members->p.release_buffer();
  }

  GammaNN::object GammaNN::operator[](UI index) {
    {
      UI i = get_series_size();
      if (i <= index) {
        members->series.gen_data.reserve(members->series.gen_data.size() + (index - i) + 1);

        My::matrix< double > input(get_object_dimention(), get_units_number() + get_trace_size());
        for (; i <= index; i++) {
          for (UI j = 0; j < get_units_number(); j++) {
            input[j] = members->units[j][i - 1];
          }
          for (UI j = get_units_number(), obj_number = 1; j < input.height(); j++, obj_number++) {
            input[j] = members->series[i - obj_number];
          }

          members->series.gen_data.emplace_back(members->p(input.get_vec()));

          std::for_each(members->units.begin(), members->units.end(), [](GammaUnit& unit) { unit.next(); } );
        }
      }
    }

    return members->series[index];
  }

  GammaNN::GammaNN(const GammaNN& nn) {
    (*this) = nn;
  }
  GammaNN::GammaNN(GammaNN&& nn) {
    (*this) = std::move(nn);
  }
  GammaNN& GammaNN::operator=(const GammaNN& nn) {
    members.reset(new GammaNN_members(*nn.members));
    return *this;
  }
  GammaNN& GammaNN::operator=(GammaNN&& nn) {
    GammaNN tmp;
    members = std::move(nn.members);
    nn.members = std::move(tmp.members);
    return *this;
  }

#define SERIALIZE_MEMBERS(STREAM, MEMBERS)                       \
  STREAM                                                         \
    & const_cast<My::matrix< double >&>(MEMBERS->series.src_data)\
    & MEMBERS->series.gen_data                                   \
    & MEMBERS->units                                             \
    & MEMBERS->trace_size                                        \
    & MEMBERS->p

  void GammaNN::write_to_stream(std::ostream& os) const {
    SERIALIZE_MEMBERS(os, members);
  }
  GammaNN GammaNN::from_stream(std::istream& is) {
    GammaNN nn;
    SERIALIZE_MEMBERS(is, nn.members);
    for (auto& unit : nn.members->units) {
      unit.series_ptr = &nn.members->series;
    }
    return std::move(nn);
  }
}

#endif
