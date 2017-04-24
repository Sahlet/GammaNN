#include<assert.h>
#include<My/Perceptron.h>
#include <My/matrix.h>
#include<fstream>
#include<iostream>
#include<ctime>

int main() {

	//auto seed = std::time(nullptr) % 1000;
	//std::cout << seed << std::endl;
	//srand(seed);

	//XOR test
	My::Perceptron p(2, 1, { 2 });
	std::vector< std::pair < std::vector< double >, std::vector< double > > > patterns {
		{
			{ 0, 0 },{ 0 }
		}
		,
		{
			{ 0, 1 },{ 1 }
		}
		,
		{
			{ 1, 0 },{ 1 }
		}
		,
		{
			{ 1, 1 },{ 0 }
		}
	};
	
	const double eps = 0.00001;
	int epochs = 0;
	double err;

	do {
		err = p.back_prop(patterns);
		epochs++;
	} while (p.back_prop(patterns) > eps);

	std::cout << "epochs = " << epochs << std::endl;

	for (auto& pattern : patterns) {
		assert(std::abs(p(pattern.first)[0] - pattern.second[0]) <= std::pow(2*eps, 0.5));
		std::cout << "[" << pattern.first[0] << ", " << pattern.first[1] << "] -> " << p(pattern.first)[0] << std::endl;
	}


	system("pause");
    return 0;
}