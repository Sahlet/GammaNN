#ifndef __PERCEPTRON_HPP__
#define __PERCEPTRON_HPP__

#include <vector>
#include <list>
#include <ostream>
#include <istream>
#include <memory>
#include <stdexcept>

#include <My/matrix.h>

namespace My {

	class Perceptron {
		std::vector< matrix< double > > weights;
		
		//there is some problems with training NN with linear outputs
		//bool last_is_linear = false;

		struct flushable {
			std::vector< matrix< double > > weights_gradients;
			std::vector< std::vector< double > > outputs;
		};
		std::unique_ptr< flushable > context;

		template< template< class > class Container, class T >
		inline static std::vector< T > to_vector(const Container< T >& container) throw (std::out_of_range) {
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
		void move(Perceptron& p) noexcept;
	public:
		Perceptron(
			US inputs = 0,
			US outputs = 0,
			const std::vector< US >& hidden = std::vector< US >()
			//,bool linear_outs = false
		) throw (std::invalid_argument);
		Perceptron(const Perceptron& p) { this->copy(p); }
		Perceptron(Perceptron&& p) noexcept { this->move(p); }
		~Perceptron();

		Perceptron& operator=(const Perceptron& p) { this->copy(p); }
		Perceptron& operator=(Perceptron&& p) {
			this->move(p);
			return *this;
		}

		int inputs_count() const;
		int outputs_count() const;

		std::vector< double > operator()(std::vector< double > input) const throw (std::invalid_argument);
		std::vector< double > forward_prop(std::vector< double > input) throw (std::invalid_argument);

		void flush();
		bool flushed() { return !context; }
		//bool is_last_linear() { return last_is_linear; }

		//errors in back_prop method
		typedef std::vector< double > errors;

		//for building composite structures where this object is intermediate
		//-- parameter 'errors' is errors that passing to outputs of perceptron (back_prop method)
		//-- parameter 'flush' responds for changing weights: if flush is true - weights changes in the function; if flush is false - gradients of weights will accumulate
		//-- result of function is errors of perceptron inputs (vector of inputs errors = (SCALAR_PRODUCT((deltas next layer),weights(from input to next layer)) where input = 1:inputs_count))
		errors put_errors(errors e, bool flush = true) throw (std::runtime_error);

		struct pattern {
			const std::vector< double >& input;
			const std::vector< double >& output;

			pattern(const std::pair< std::vector< double >, std::vector< double > >& data) :
				input(data.first), output(data.second) {}
		};

		//returns sum of ERROR values
		double back_prop(const std::vector< pattern >& patterns) throw (std::invalid_argument);

		//returns ERROR value
		double back_prop(const pattern& p) throw (std::invalid_argument) {
			return back_prop(std::vector< pattern >{ p });
		}

		//returns sum of ERROR values
		double back_prop(const std::vector< std::pair < std::vector< double >, std::vector< double > > >& patterns1) throw (std::out_of_range, std::invalid_argument) {
			std::vector< My::Perceptron::pattern > patterns;
			patterns.reserve(patterns1.size());
			for (auto& pattern : patterns1) {
				patterns.emplace_back(pattern);
			}
			return back_prop(patterns);
		}

		void write_to_stream(std::ostream& os) const;
		static Perceptron from_stream(std::istream& is);
	};
}

std::ostream& operator & (std::ostream& os, const My::Perceptron& p);

std::istream& operator & (std::istream& is, My::Perceptron& p);

#endif