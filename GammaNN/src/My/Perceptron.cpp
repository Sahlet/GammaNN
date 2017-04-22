#ifndef __PERCEPTRON_CPP__
#define __PERCEPTRON_CPP__

#include "Perceptron.h"
#include <iostream>

#ifndef __PRETTY_FUNCTION__
#define __PRETTY_FUNCTION__ __func__
#endif

namespace My {

	namespace Context {
		struct flushable : Perceptron::flushable {
			std::vector< matrix< double > > weights_gradients;
		};
	}

	Perceptron::Perceptron(
		int intputs,
		std::vector< int > hidden,
		int outs,
		bool linear_outs)
	{
		std::cout << __PRETTY_FUNCTION__ << std::endl << std::flush;
	}

	Perceptron::~Perceptron() {}

	void Perceptron::flush() {}
	Perceptron::deltas Perceptron::put_deltas(deltas d, bool flush) {
		return deltas();
	}

	void Perceptron::copy(const Perceptron& p) {}
	void Perceptron::move(Perceptron& p) {}

	double Perceptron::back_prop(const std::vector< back_prop_param >& params) {
		return 0;
	}

	std::vector< double > Perceptron::operator()(const std::vector< double >& input) const {
		std::vector< double > output;

		return std::move(output);
	}

	void Perceptron::write_to_stream(std::ostream& os) const {}
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