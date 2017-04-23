#include<assert.h>
#include<My/Perceptron.h>
#include <My/matrix.h>

int main() {

	My::matrix< double > m({ {1, 2}, {3, 4} });
	m *= std::vector<double>{2, 2};

	assert(m == My::matrix< double >({ {6, 14} }));

	//My::Perceptron p(3);

    return 0;
}