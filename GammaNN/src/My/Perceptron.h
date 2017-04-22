#ifndef __PERCEPTRON_HPP__
#define __PERCEPTRON_HPP__

#include <vector>
#include <list>
#include <ostream>
#include <istream>
#include <memory>

#include "matrix.h"

namespace My {

	struct Perceptron {
		struct flushable {};
	private:
		std::vector< matrix< double > > weights;
		std::unique_ptr< flushable > context;
		bool last_is_linear = false;

		template< template< class > class Container, class T >
		inline static std::vector< T > to_vector(const Container< T >& container) {
			auto size = container.size();
			std::vector< T > vec;
			vec.capacity(size);
			int i = 0;
			for (auto& obj : container) {
				if (i > size) throw std::out_of_range();
				vec.emplace_back(std::forward< T >(obj));
			}
			return std::move(vec);
		}
		
		void copy(const Perceptron& p);
		void move(Perceptron& p);
	public:
		Perceptron(
			int intputs = 1,
			std::vector< int > hidden = std::vector< int >(),
			int outs = 1,
			bool linear_outs = false
		);
		Perceptron(const Perceptron& p) { this->copy(p); }
		Perceptron(Perceptron&& p) { this->move(p); }
		~Perceptron();

		Perceptron& operator=(const Perceptron& p) { this->copy(p); }
		Perceptron& operator=(Perceptron&& p) {
			this->move(p);
			return *this;
		}

		template< template< class > class Container >
		Perceptron(
			int intputs,
			const Container< int >& hidden,
			int outs = 1,
			bool linear_outs = false
		) throw (std::out_of_range) : Perceptron(intputs, std::move(to_vector(hidden)), outs, linear_outs) {}

		void flush();
		bool flushed() { return !context; }

		std::vector< double > operator()(const std::vector< double >& input) const;

		struct pattern {
			const std::vector< double >& input;
			const std::vector< double >& output;
		};

		//deltas in back_prop method
		typedef std::unique_ptr< std::vector< double > > deltas;

		//for building composite structures where this object is intermediate
		//-- parameter 'deltas' is deltas that passing to outputs of perceptron (back_prop method)
		//-- parameter 'flush' responds for changing weights: if flush is true - weights changes in the function; if flush is false - gradients of weights will accumulate
		//-- result of function is deltas of perceptron inputs
		deltas put_deltas(deltas d, bool flush = true);

		struct back_prop_param : pattern, deltas {};

		//returns ERROR value
		double back_prop(const back_prop_param& param) {
			return back_prop({ param });
		}

		//returns sum of ERROR values
		template< template< class > class Container >
		double back_prop (const Container< back_prop_param >& params) throw (std::out_of_range) {
			return back_prop(std::move(to_vector(params)));
		}
		
		//returns sum of ERROR values
		double back_prop(const std::vector< back_prop_param >& params);

		void write_to_stream(std::ostream& os) const;
		static Perceptron from_stream(std::istream& is);
	};
}

std::ostream& operator << (std::ostream& os, const My::Perceptron& p);

std::istream& operator >> (std::istream& is, My::Perceptron& p);

#endif