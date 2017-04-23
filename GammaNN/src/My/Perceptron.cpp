#ifndef __PERCEPTRON_CPP__
#define __PERCEPTRON_CPP__

#include "Perceptron.h"
#include <iostream>

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

		std::vector< double > operator-(const std::vector< double >& v) {
			std::vector< double > res(v);
			for (auto& obj : res) { obj = -obj; }
			return std::move(res);
		}
		std::vector< double > operator-(const std::vector< double >& v1, const std::vector< double >& v2) {
			if (v1.size() != v2.size()) throw std::invalid_argument("v1 and v2 have different dimensions");
			std::vector< double > res(v1.size());
			for (int i = 0; i < v1.size(); i++) { res[i] = v1[i] - v2[i]; }
			return std::move(res);
		}
		std::vector< double > operator+(const std::vector< double >& v1, const std::vector< double >& v2) {
			if (v1.size() != v2.size()) throw std::invalid_argument("v1 and v2 have different dimensions");
			std::vector< double > res(v1.size());
			for (int i = 0; i < v1.size(); i++) { res[i] = v1[i] + v2[i]; }
			return std::move(res);
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

		/*for () {
		}*/

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
		if (!patterns.size()) return;
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

		return global_error;
	}

	void Perceptron::write_to_stream(std::ostream& os) const {

	}
	Perceptron Perceptron::from_stream(std::istream& is) { return Perceptron(); }
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