#include<assert.h>
#include<My/Perceptron.h>
#include <My/matrix.h>
#include<fstream>
#include<iostream>

int main() {

	My::matrix< double > m({ {1, 2}, {3, 4} });
	m *= std::vector<double>{2, 2};

	assert(m == My::matrix< double >({ {6, 14} }));

	//XOR test
	My::Perceptron p(2, 1, { 2 });
	std::vector< std::pair < std::vector< double >, std::vector< double > > > patterns {
		{
			{ 0, 0 },{ 0 }
		},
		{
			{ 0, 1 },{ 1 }
		},
		{
			{ 1, 0 },{ 1 }
		},
		{
			{ 1, 1 },{ 0 }
		}
	};
	
	double eps = 0.1;
	while (p.back_prop(patterns) > eps) {}

	for (auto& pattern : patterns) {
		assert(std::abs(p(pattern.first)[0] - pattern.second[0]) <= eps);
	}



    return 0;
}