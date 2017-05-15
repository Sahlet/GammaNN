#ifndef __PERCEPTRON_CPP__
#define __PERCEPTRON_CPP__

#include <My/Perceptron.h>
#include <istream>
#include <ostream>
#include <type_traits>

#ifndef __PRETTY_FUNCTION__
#define __PRETTY_FUNCTION__ __func__
#endif

#define LINEAR_OUTS true
#define BACK_PROP_WITH_REGULARIZATION false
#define BACK_PROP_WITH_RANDOM true
#define DEBUG_MODE true

namespace My {

	namespace {
		inline double sigmoid(double x) {
			return 1 / (1 + std::exp(-x));
		}

		inline double Dsigmoid(double x) {
			double value = std::exp(-x);
			return value * std::pow(1 + value, -2);
		}

		inline std::vector< double > use_for_vec(std::vector< double > X, double f(double x)) {
			if (f) for (auto& x : X) {
				x = f(x);
			}
			return std::move(X);
		}
#define VEC_OPERATIONS(NAME, OPERATOR)\
		std::vector< double > NAME(const std::vector< double >& v1, const std::vector< double >& v2) {\
			if (v1.size() != v2.size()) throw std::invalid_argument("v1 and v2 have different dimensions");\
			std::vector< double > res(v1.size());\
			for (int i = 0; i < v1.size(); i++) { res[i] = v1[i] OPERATOR v2[i]; }\
			return std::move(res);\
		}\
		std::vector< double > NAME(const std::vector< double >& v1, const double& v2) {\
			std::vector< double > res(v1.size());\
			for (int i = 0; i < v1.size(); i++) { res[i] = v1[i] OPERATOR v2; }\
			return std::move(res);\
		}\
		std::vector< double > NAME(const double& v1, const std::vector< double >& v2) {\
			std::vector< double > res(v2.size());\
			for (int i = 0; i < v2.size(); i++) { res[i] = v1 OPERATOR v2[i]; }\
			return std::move(res);\
		}

		VEC_OPERATIONS(operator+, +);
		VEC_OPERATIONS(operator-, -);
		VEC_OPERATIONS(operator*, *);

		double operator,(const std::vector< double >& v1, const std::vector< double >& v2) {
		  if (v1.size() != v2.size()) throw std::invalid_argument("v1 and v2 have different dimensions");
		  double res = 0;
		  for (int i = 0; i < v1.size(); i++) { res += v1[i] * v2[i]; }
		  return res;
		}

		std::vector< double > operator-(const std::vector< double >& v) {
			return v * -1;
		}

		//-----------------------------------------------------------------------
		//SERIALIZE
		template< class T >
		typename std::enable_if<std::is_fundamental<T>::value || std::is_enum<T>::value, std::ostream& >::type
			operator&(std::ostream& os, const T& v) {
			os.write((const char*)&v, sizeof(T));
			return os;
		}
		template< class T >
		typename std::enable_if<std::is_fundamental<T>::value || std::is_enum<T>::value, std::istream& >::type
			operator&(std::istream& is, T& v) {
			is.read((char*)&v, sizeof(T));
			return is;
		}
		template< class T >
		typename std::enable_if<std::is_compound<T>::value && !std::is_enum<T>::value, std::ostream& >::type
			operator&(std::ostream& os, const T& obj);
		template< class T >
		typename std::enable_if<std::is_compound<T>::value && !std::is_enum<T>::value, std::istream& >::type
			operator&(std::istream& is, T& obj);

		//-----------------------------------------------------------------------
		//VECTOR SERIALIZE
		template< class T >
		void serialize(std::ostream& os, const std::vector< T >& v) {
			os & v.size();
			for (auto& obj : v) { os & obj; }
		}
		template< class T >
		void serialize(std::istream& is, std::vector< T >& v) {
			typename std::vector< T >::size_type size;
			is & size;
			v.resize(size);
			for (auto& obj : v) { is & obj; }
		}

		//-----------------------------------------------------------------------
		//MATRIX SERIALIZE
		template< class T >
		void serialize(std::ostream& os, const matrix< T >& m) {
			os & m.width() & m.height();
			for (auto& obj : m) { os & obj; }
		}
		template< class T >
		void serialize(std::istream& is, matrix< T >& m) {
			US w, h;
			is & w & h;
			m = matrix< T >(w, h);
			for (auto& obj : m) { is & obj; }
		}

		//-----------------------------------------------------------------------
		//SERIALIZE
		template< class T >
		typename std::enable_if<std::is_compound<T>::value && !std::is_enum<T>::value, std::ostream& >::type
		  operator&(std::ostream& os, const T& obj) {
		    serialize(os, obj);
		    return os;
		}
		template< class T >
		typename std::enable_if<std::is_compound<T>::value && !std::is_enum<T>::value, std::istream& >::type
		  operator&(std::istream& is, T& obj) {
		    serialize(is, obj);
		    return is;
		}

	}

	void Perceptron::copy(const Perceptron& p) {
		if (this == &p) return;
		weights = p.weights;

		context.reset();
		if (p.context) {
			context.reset(new flushable(*p.context));
		}
	}
	void Perceptron::move(Perceptron& p) noexcept {
		if (this == &p) return;
		weights = std::move(p.weights);
		p.weights.resize(1);

		context = std::move(p.context);
	}

	Perceptron::Perceptron(
		US inputs,
		US outputs,
		const std::vector< US >& hidden
	) throw (std::invalid_argument)
	{
		if (!(inputs && outputs)) {
			weights.resize(1);
			weights[0] = matrix<double>(0, 1);
			return;
		}

		for (int layer_size : hidden) {
			if (!layer_size) throw std::invalid_argument("Perceptron(): layer_size == 0");
		}

		weights.resize(1 + hidden.size());

		int height = inputs + 1;

		for (int i = 0; i <= hidden.size(); i++) {
			int width = (i == hidden.size()) ? outputs : hidden[i];

			weights[i] = matrix< double >(width, height);

			// for (auto& weight : weights[i]) {
			// weight = (1 - 2*(std::rand()%2)) * (0.1 + (std::rand() % 10000) / 1000.0);
			// }

			//for (auto& weight : weights[i]) {
			//	weight = (1 - 2 * (std::rand() % 2)) * (0.1 + (std::rand() % 1000) / 5000.0);
			//}

			for (auto& weight : weights[i]) {
				weight = (0.1 + (std::rand() % 10000) / 20000.0);
			}

			height = width + 1;
		}
	}

	Perceptron::~Perceptron() {}

	int Perceptron::inputs_count() const { return weights.front().height() - 1; }
	int Perceptron::outputs_count() const { return weights.back().width(); }

#define BIAS_COEFF 1

	std::vector< double > Perceptron::operator()(std::vector< double > input) const throw (std::invalid_argument) {
		if (input.size() != inputs_count()) throw std::invalid_argument("input.size() != inputs_count()");
		for (const auto& weight : weights) {
			input.push_back(BIAS_COEFF);
			input = use_for_vec(
				input * weight,
				(LINEAR_OUTS && (&weight == &weights.back())) ? nullptr : sigmoid
			);
		}
		return std::move(input);
	}

	std::vector< double > Perceptron::forward_prop(std::vector< double > input) throw (std::invalid_argument) {
		if (input.size() != inputs_count()) throw std::invalid_argument("input.size() != inputs_count()");
		if (!context) context.reset(new flushable);
		context->outputs.resize(1 + weights.size());

		std::vector< double >* outputs_ptr = context->outputs.data();

		for (const auto& weight : weights) {
			input.push_back(BIAS_COEFF);
			input = use_for_vec(
				(*outputs_ptr++ = std::move(input)) * weight,
				(LINEAR_OUTS && (&weight == &weights.back())) ? nullptr : sigmoid
			);
		}
		*outputs_ptr = input;
		return std::move(input);
	}

	Perceptron::errors Perceptron::put_errors(errors e, bool flush) throw (std::runtime_error) {
		if (e.size() != outputs_count()) throw (std::invalid_argument("e.size() != outputs_count()"));
		if (!context || context->outputs.size() != (weights.size() + 1)) throw (std::runtime_error("wrong context"));

		context->weights_gradients.resize(weights.size());
		context->last_batch_size++;

		auto w_iter = weights.rbegin();
		auto g_iter = context->weights_gradients.rbegin();
		auto o_iter = context->outputs.rbegin(), pred_o_iter = std::next(o_iter);

		for (; w_iter != weights.rend(); w_iter++, g_iter++, o_iter++, pred_o_iter++) {

		  if (g_iter->width() != w_iter->width() || g_iter->height() != w_iter->height()) {
		    *g_iter = matrix< double >(w_iter->width(), w_iter->height());
		  }

			if (!(LINEAR_OUTS && (&*w_iter == &weights.back()))) {
			  e = *o_iter * (1 - *o_iter) * e;
			}

			*g_iter +=
				matrix< double >(1, pred_o_iter->size(), *pred_o_iter)
				*
				matrix< double >(e.size(), 1, e);

			e = *w_iter * e;

			pred_o_iter->resize(pred_o_iter->size() - 1);
			e.resize(e.size() - 1);
		}

		if (flush) {
			this->flush();
		}

		return std::move(e);
	}

	void Perceptron::flush() {
		if (flushed()) return;
		int last_batch_size = context->last_batch_size;
		context->last_batch_size = 0;
		if (!context->weights_gradients.size()) return;
		context->weights_gradients_accumulator.resize(context->weights_gradients.size());

		auto static add_rand = [](matrix< double >& derives) {
		  const static int rand_mod = 10000;
		  const static double rand_coeff = 0.01;

		  for (auto& value : derives) {
		    value += (
                 (
                    (std::rand() % rand_mod)/double(rand_mod) - 0.5
                 ) * rand_coeff
		    );
		  }
		};

		auto static add_regularization = [](const matrix< double >& weight, matrix< double >& derives) {
		  const static double regularization_param = 0.001;

		  for (int i = 0; i < (weight.height() - 1); i++) {
		    for (int j = 0; j < weight.width(); j++) {
		      derives(i, j) += weight(i, j) * regularization_param;
		    }
		  }
		};

		const double speed = 1;
		const double accumulation = 0.2;

		auto iter = context->weights_gradients.begin();
		auto accumulator_iter = context->weights_gradients_accumulator.begin();
		for (auto& weight : weights) {

#if BACK_PROP_WITH_REGULARIZATION
		  add_regularization(weight, *iter);
#endif

#if BACK_PROP_WITH_RANDOM
		  add_rand(*iter);
#endif
		  (*iter) *= (1.0 / last_batch_size);

		  if (accumulator_iter->dimension() == iter->dimension()) {
		    *accumulator_iter *= accumulation;
		    *accumulator_iter += (1 - accumulation) * (*iter);
		  } else {
		    *accumulator_iter = *iter;
		  }

		  matrix< double >& derives = *accumulator_iter;
		  weight -= speed * derives;

#if DEBUG_MODE
		  for (auto& w : weight) {
		    if (std::abs(w) > 100000) {
		      printf("WARNING in Perceptron.cpp: std::abs(weight) > 100000\n");
		    }
		  }
#endif

		  *iter = 0;
		  iter++;
		  accumulator_iter++;
		}
	}

	double Perceptron::back_prop(const std::vector< pattern >& patterns) throw (std::invalid_argument) {
		if (!patterns.size()) return 0;
		for (const auto& p : patterns) {
			if ((p.input.size() != inputs_count()) || (p.output.size() != outputs_count()))
				throw std::invalid_argument("some of patterns has wrong size");
		}

		double global_error = 0;

		for (const auto& p : patterns) {
			errors e = forward_prop(p.input) - p.output;
		  global_error += (e, e);
			put_errors(e, false);
		}

		flush();

		return global_error / 2;
	}

	void Perceptron::write_to_stream(std::ostream& os) const {
		bool there_is_context = (bool)context;

		os & weights & there_is_context;

		if (there_is_context) {
			os & context->weights_gradients & context->outputs & context->last_batch_size;
		}
	}
	Perceptron Perceptron::from_stream(std::istream& is) {
		Perceptron p;
		bool there_is_context;

		is & p.weights & there_is_context;

		if (there_is_context) {
			p.context.reset(new flushable);
			is & p.context->weights_gradients & p.context->outputs & p.context->last_batch_size;
		}
		return std::move(p);
	}
}

std::ostream& operator << (std::ostream& os, const My::Perceptron& p) {
	p.write_to_stream(os);
	return os;
}

std::istream& operator >> (std::istream& is, My::Perceptron& p) {
	p = std::move(My::Perceptron::from_stream(is));
	return is;
}

#endif
