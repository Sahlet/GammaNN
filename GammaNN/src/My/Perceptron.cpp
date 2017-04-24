#ifndef __PERCEPTRON_CPP__
#define __PERCEPTRON_CPP__

#include "Perceptron.h"
#include <istream>
#include <ostream>
#include <type_traits>


#ifndef __PRETTY_FUNCTION__
#define __PRETTY_FUNCTION__ __func__
#endif

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
			if (!f) for (auto& x : X) {
				x = f(x);
			}
			return std::move(X);
		}

#define VEC_OPERATIONS(OPERATOR)\
		std::vector< double > operator##OPERATOR(const std::vector< double >& v1, const std::vector< double >& v2) {\
			if (v1.size() != v2.size()) throw std::invalid_argument("v1 and v2 have different dimensions");\
			std::vector< double > res(v1.size());\
			for (int i = 0; i < v1.size(); i++) { res[i] = v1[i] OPERATOR v2[i]; }\
			return std::move(res);\
		}\
		std::vector< double >& operator##OPERATOR##=(std::vector< double >& v1, const double& v2) {\
			for (int i = 0; i < v1.size(); i++) { v1[i] OPERATOR##= v2; }\
			return v1;\
		}\
		std::vector< double >& operator##OPERATOR##=(std::vector< double >& v1, const std::vector< double >& v2) {\
			if (v1.size() != v2.size()) throw std::invalid_argument("v1 and v2 have different dimensions");\
			for (int i = 0; i < v1.size(); i++) { v1[i] OPERATOR##= v2[i]; }\
			return v1;\
		}\
		std::vector< double > operator##OPERATOR(const std::vector< double >& v1, const double& v2) {\
			std::vector< double > res(v1.size());\
			for (int i = 0; i < v1.size(); i++) { res[i] = v1[i] OPERATOR v2; }\
			return std::move(res);\
		}\
		std::vector< double > operator##OPERATOR(const double& v1, const std::vector< double >& v2) {\
			return v2 OPERATOR v1;\
		}

		VEC_OPERATIONS(+);
		VEC_OPERATIONS(-);
		VEC_OPERATIONS(*);

		std::vector< double > operator-(const std::vector< double >& v) {
			return v * -1;
		}

		//-----------------------------------------------------------------------
		//SERIALIZE
		template< class T >
		typename std::enable_if_t<std::is_fundamental<T>::value || std::is_enum<T>::value, std::ostream& >
			operator&(std::ostream& os, const T& v) {
			os.write((const char*)&v, sizeof(T));
			return os;
		}
		template< class T >
		typename std::enable_if_t<std::is_fundamental<T>::value || std::is_enum<T>::value, std::istream& >
			operator&(std::istream& is, T& v) {
			is.read((char*)&v, sizeof(T));
			return is;
		}
		template< class T >
		typename std::enable_if_t<std::is_compound<T>::value && !std::is_enum<T>::value, std::ostream& >
			operator&(std::ostream& os, const T& obj) {
			serialize(os, obj);
			return os;
		}
		template< class T >
		typename std::enable_if_t<std::is_compound<T>::value && !std::is_enum<T>::value, std::istream& >
			operator&(std::istream& is, T& obj) {
			serialize(is, obj);
			return is;
		}
		
		//-----------------------------------------------------------------------
		//VECTOR SERIALIZE
		template< class T >
		void serialize(std::ostream& os, const std::vector< T >& v) {
			os & v.size();
			for (auto& obj : v) { os & obj; }
		}
		template< class T >
		void serialize(std::istream& is, std::vector< T >& v) {
			std::vector< T >::size_type size;
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

	}

	void Perceptron::copy(const Perceptron& p) {
		if (this == &p) return;
		weights = p.weights;
		last_is_linear = p.last_is_linear;
		
		context.reset();
		if (p.context) {
			context.reset(new flushable(*p.context));
		}
	}
	void Perceptron::move(Perceptron& p) noexcept {
		if (this == &p) return;
		weights = std::move(p.weights);
		p.weights.resize(1);
		last_is_linear = p.last_is_linear;

		context = std::move(p.context);
	}

	Perceptron::Perceptron(
		US inputs,
		US outputs,
		const std::vector< US >& hidden,
		bool linear_outs) throw (std::invalid_argument)
	{
		if (!(inputs && outputs)) {
			weights.resize(1);
			return;
		}

		for (int layer_size : hidden) {
			if (layer_size <= 0) throw std::invalid_argument("Perceptron(): layer_size == 0");
		}

		weights.resize(1 + hidden.size());

		int height = inputs;
		
		for (int i = 0; i <= hidden.size(); i++) {
			int width = (i == hidden.size()) ? outputs : hidden[i];
			
			weights[i] = matrix< double >(width, height);

			for (auto& weight : weights[i]) {
				weight = (1 - 2*(std::rand()%2)) * (1 + (std::rand() % 100) / 10.0);
			}

			height = width;
		}
	}

	Perceptron::~Perceptron() {}

	std::vector< double > Perceptron::operator()(std::vector< double > input) const throw (std::invalid_argument) {
		if (input.size() != inputs_count()) throw std::invalid_argument("input.size() != inputs_count()");
		for (const auto& weight : weights) {
			input = use_for_vec(
				input * weight,
				(last_is_linear && &weight == &weights.back()) ? nullptr : sigmoid
			);
		}
		return std::move(input);
	}

	std::vector< double > Perceptron::forward_prop(std::vector< double > input) throw (std::invalid_argument) {
		if (input.size() != inputs_count()) throw std::invalid_argument("input.size() != inputs_count()");
		if (!context) context.reset(new flushable);
		context->outputs.resize(1 + weights.size());

		auto outputs_ptr = context->outputs.data();
		for (const auto& weight : weights) {
			input = use_for_vec(
				(*outputs_ptr++ = std::move(input)) * weight,
				(last_is_linear && &weight == &weights.back()) ? nullptr : sigmoid
			);
		}
		*outputs_ptr = input;
		return std::move(input);
	}

	Perceptron::errors Perceptron::put_errors(errors e, bool flush) throw (std::runtime_error) {
		if (e.size() != outputs_count()) throw (std::invalid_argument("e.size() != outputs_count()"));
		if (!context || context->outputs.size() != weights.size()) throw (std::runtime_error("wrong context"));

		context->weights_gradients.resize(weights.size());

		auto w_iter = weights.rbegin();
		auto g_iter = context->weights_gradients.rbegin();
		auto o_iter = context->outputs.rbegin(), pred_o_iter = std::next(o_iter);

		const double speaad = 1;

		for (; w_iter != weights.rend(); w_iter++, g_iter++, o_iter++, pred_o_iter++) {
			
			if (last_is_linear && &*w_iter == &weights.back()) e *= -1;
			else e *= *o_iter * (1 - *o_iter);

			if (g_iter->width() != w_iter->width() || g_iter->height() != w_iter->height()) {
				*g_iter = matrix< double >(w_iter->width(), w_iter->height());
			}

			*g_iter -= speaad * (
				matrix< double >(1, pred_o_iter->size(), *pred_o_iter)
				*
				matrix< double >(e.size(), 1, e)
				);

			e = *w_iter * e;
		}

		context->outputs.clear();

		if (flush) {
			this->flush();
		}

		return std::move(e);
	}

	void Perceptron::flush() {
		if (!context) return;
		if (!context->weights_gradients.size()) return;
		
		auto iter = context->weights_gradients.begin();
		for (auto& weight : weights) {
			weight += *iter++;
		}

		context.reset();
	}

	double Perceptron::back_prop(const std::vector< pattern >& patterns) throw (std::invalid_argument) {
		if (!patterns.size()) return 0;
		for (const auto& p : patterns) {
			if ((p.input.size() != inputs_count()) || (p.output.size() != outputs_count()))
				throw std::invalid_argument("some of patterns has wrong size");
		}

		double global_error = 0;

		for (const auto& p : patterns) {
			errors e = p.output - forward_prop(p.input);
			for (const auto& error : e) {
				global_error += error * error;
			}
			put_errors(e, false);
		}

		flush();

		return global_error / 2;
	}

	void Perceptron::write_to_stream(std::ostream& os) const {
		bool there_is_context = (bool)context;

		os & weights & last_is_linear & there_is_context;

		if (there_is_context) {
			os & context->weights_gradients & context->outputs;
		}
	}
	Perceptron Perceptron::from_stream(std::istream& is) {
		Perceptron p;
		bool there_is_context;

		is & p.weights & p.last_is_linear & there_is_context;

		if (there_is_context) {
			p.context.reset(new flushable);
			is & p.context->weights_gradients & p.context->outputs;
		}
		return std::move(p);
	}
}

std::ostream& operator & (std::ostream& os, const My::Perceptron& p) {
	p.write_to_stream(os);
	return os;
}

std::istream& operator & (std::istream& is, My::Perceptron& p) {
	p = std::move(My::Perceptron::from_stream(is));
	return is;
}

#endif