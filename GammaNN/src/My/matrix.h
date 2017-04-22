//matrix.h
#pragma once
#include <My\string.h>
#include <vector>
#include <set>
#include <algorithm>
#include <stdexcept>

using namespace std;

namespace My{

extern "C++"{
template<class T>
vector<T> operator+(const vector<T>& l, const vector<T>& r) throw(exception);/*{
	if (l.size() != r.size()) throw(runtime_error("bad size of vector"));
	vector<T> res(l.size());
	auto k = res.begin(), end_k = res.end();
	for (auto i = l.begin(), j = r.begin(); k != end_k; (*k++ = *i++ + *j++));
	return res;
}*/
template<class T>
vector<T> operator-(const vector<T>& v) throw(exception);/*{
	vector<T> res(v.size());
	auto k = res.begin(), end_k = res.end();
	for (auto i = v.begin(); k != end_k; (*k++ = - *i++));
	return res;
}*/
template<class T>
vector<T> operator-(const vector<T>& l, const vector<T>& r) throw(exception);/*{
	return l + (-r);
}*/
//????????? ????????????
template<class T>
T operator,(const vector<T>& l, const vector<T>& r) throw(exception);/*{
	if (l.size() != r.size()) throw(runtime_error("bad size of vectors"));
	T res = T();
	for (auto i1 = l.begin(), i2 = r.begin(), end1 = l.end(); i1 != end1; i1++, i2++) res += *i1 * *i2;
	return res;
}*/
template<class T>
string to_string(const vector<T>& v);/*{
	return My::to_string(matrix<T>(v).get_transplanted());
}*/


//template<class T>
//extern
//matrix<T>abs_ matrix<T>abs_ptr;

}

//???? ?????? ???????????? matrix::abs - ?? ???????? ??????????
//matrix<Type_name>::abs_ matrix<Type_name>::abs_ptr = matrix<Type_name>::std_abs;
//Type_name - ??? ????
template<class T = long double>
class matrix : protected vector< vector<T> >{
	typedef T (*abs_)(const matrix&);
	static abs_ abs_ptr;
public:
	matrix(const matrix&) = default;
	static abs_ set_abs(abs_ value){
		abs_ res = abs_ptr;
		abs_ptr = value;
		return res;
	}
	typedef vector< vector<T> > base;
	class iterator{
		friend class matrix;
		typedef typename vector< vector< T > >::iterator iter;
		typedef typename vector< T >::iterator sub_iter;
		const iter end;
		iter courent;
		sub_iter sub_courent;
		iterator(matrix& m) : courent(m.base::begin()), end(m.base::end()){
			if (m.width() == 0) courent = end;
			if (courent != end) sub_courent = courent->begin();
		}
	public:
		bool is_end(){
			return courent == end;
		}
		iterator get_end(){
			iterator res(*this);
			res.courent = res.end;
			return res;
		}
		//????
		iterator& operator++() throw(exception){
			if (courent == end) throw std::range_error("out of range");
			if(++sub_courent == courent->end()){
				courent++;
				if (courent != end) sub_courent = courent->begin();
				else sub_courent = sub_iter();
			}
			return (*this);
		}
		iterator operator++(int) throw(exception){
			auto res = (*this);
			++(*this);
			return res;
		}
		bool operator==(const iterator& r) throw(exception){
			if(this->courent == r.courent && this->end == r.end){
				if (this->courent == this->end) return true;
				return this->sub_courent == r.sub_courent;
			}
			return false;
		}
		bool operator!=(const iterator& r) throw(exception){
			return !((*this) == r);
		}
		T& operator*(){
			if (courent == end) throw std::range_error("out of range");
			return *sub_courent;
		}
		T* operator->(){
			if (courent == end) throw std::range_error("out of range");
			return sub_courent.operator->();
		}
	};
	class const_iterator : protected iterator{
		friend class matrix;
		const_iterator (matrix& m) : iterator(m){}
		const_iterator (const iterator& i) : iterator(i){}
	public:
		bool operator==(const const_iterator& r) throw(exception){
			return this->iterator::operator==(r);
		}
		bool operator!=(const iterator& r) throw(exception){
			return !((*this) == r);
		}
		const_iterator& operator++() throw(exception){
			this->iterator::operator++();
			return *this;
		}
		const_iterator operator++(int) throw(exception){
			return this->iterator::operator++(0);
		}
		T operator*(){
			return this->iterator::operator*();
		}
		const T* operator->(){
			return this->iterator::operator->();
		}
	};
	friend class iterator;
	unsigned int width() const{
		if(this->size()) return this->base::begin()->size();
		else return 0;
	}
	unsigned int height() const{ return this->size(); }
	matrix (const unsigned int& width = 0, const unsigned int& height = 0) : base(height){
		for (auto& i : (base&)(*this)) i = vector<T>(width);
	}
	matrix (const vector<T>& v) : base(1){
		*(this->base::begin()) = v;
	}
	matrix (vector<T>&& v) noexcept : base(1){
		*(this->base::begin()) = std::move_if_noexcept(v);
	}
	matrix (const base& b) throw(exception){
		if (!b.size()) return;
		auto w = b[0].size();
		for (const auto& i : b){
			if (i.size() != w) throw exception("bad format");
		}
		this->base::operator=(b);
	}
	operator vector< vector<T> >() const{
		return *this;
	}
	matrix (matrix && obj) noexcept : base((base&&)std::move(obj)){}
	matrix& operator=(matrix && obj){
		*((vector< vector<T> >*)this) = (base&&)std::move_if_noexcept(obj);
		return *this;
	}
	class for_square_brackets{
		vector<T> * const ptr;
	public:
		for_square_brackets(vector<T> * const &ptr) : ptr(ptr) {}
		T& operator[](const unsigned int& j) throw(std::range_error){
			return (*ptr)[j];
		}
		operator vector<T>(){
			return *ptr;
		}
	};

	iterator begin(){
		return iterator(*this);
	}
	iterator end(){
		return iterator(*this).get_end();
	}
	const_iterator begin() const{
		return const_iterator(*(matrix*)this);
	}
	const_iterator end() const{
		return const_iterator(*(matrix*)this).get_end();
	}

	for_square_brackets operator [](const unsigned int& i) throw(std::range_error){
		return for_square_brackets(&(this->base::operator[](i)));
	}
	const vector<T>& operator [](const unsigned int& i) const throw(std::range_error){
		return this->base::operator[](i);
	}
	T& operator()(const pair<unsigned int, unsigned int>& point) throw(std::range_error){
		return (*this)[point.first][point.second];
	}
	T& operator()(const unsigned int& width, const unsigned int& height) throw(std::range_error){
		return (*this)[width][height];
	}

	//????? ??????????????????
	matrix get_transplanted() const{
		auto w = this->width(), h = this->height();
		matrix res(h, w);
		for (auto i = 0u; i < h; i++){
			for (auto j = 0u; j < w; j++){
				res[j][i] = (*this)[i][j];
			}
		}
		return res;
	}
	//????? ??????????????????
	matrix operator~() const{
		return this->get_transplanted();
	}
	vector<T> operator*(const vector<T>& v) const throw(exception){
		vector<T> res(this->height());
		auto iter = res.begin();
		for (const auto& i : *(base*)(this)) *iter++ = (i, v);
		return res;
	}
	matrix& operator*=(const vector<T>& v) const throw(exception){
		return *this = *this * v;
	}
	matrix& operator*=(const matrix& m) throw(exception){
		if (this->width() != m.height()) throw(std::runtime_error("bad size of matix"));
		matrix res(m.width(), this->height());
		auto m_t = m.get_transplanted();
		auto res_i = res.base::begin();
		for (const auto& i : (base&)(*this)){
			auto res_j = (res_i++)->begin();
			for (const auto& j : (base&)m_t) *res_j++ = (i , j);
		}
		*this = std::move(res);
		return *this;
	}
	matrix operator*(const matrix& m) const throw(exception){
		return std::move(matrix(*this) *= m);
	}
	matrix& operator*=(const T& t) throw(exception){
		for (auto& i : (*this)) i *= t;
		return *this;
	}
	matrix operator*(const T& t) const {
		return std::move(matrix(*this) *= t);
	}
	matrix& operator+=(const T& t) {
		for (auto& i : *this) i += t;
		return *this;
	}
	matrix operator+(const T& t) const {
		return std::move(matrix(*this) += t);
	}
	matrix& operator+=(const matrix& m) throw (exception){
		if (this->width() != m.width() || this->height() != m.height()) throw exception("bad sizes");
		auto iter = m.begin();
		for (auto& i : *this) i += *iter++;
		return *this;
	}
	matrix operator+(const matrix& m) const throw(exception) {
		return std::move(matrix(*this) += m);
	}
	matrix operator-() const throw(exception){
		matrix res(*this);
		for (auto& i : res) i = -i;
		return res;
	}
	matrix& operator-=(const matrix& m) throw (exception){
		if (this->width() != m.width() || this->height() != m.height()) throw exception("bad sizes");
		auto iter = m.begin();
		for (auto& i : *this) i -= *iter++;
		return *this;
	}
	matrix operator-(const matrix& m) const throw(exception) {
		return std::move(matrix(*this) -= m);
	}
	inline bool operator==(const matrix& m){
		return this->base::operator==(m);
	}
	inline bool operator!=(const matrix& m){
		return this->base::operator!=(m);
	}

	/*??????? ||matrix|| = MAX{
		SUM{
			abs(A[i][j]) | j ? { 0 , ... , matrix.width - 1 }
		} | i ? { 0 , ... , matrix.height - 1 }
	}*/
	static T std_abs(const matrix& m) throw (exception){
		//if (m.abs_ptr && abs_ptr != abs) abs_ptr(m);
		if (m.width() == 0 || m.height() == 0) throw (range_error("empty matryx"));
		T res = 0, tmp_res;
		for (const auto& i : (const base&)m){
			tmp_res = 0;
			for (const auto& j : i) tmp_res += std::abs(j);
			res = max(res, tmp_res);
		}
		return res;
	}
	//??????? ||matrix||
	T abs() throw (exception){
		return abs_ptr(*this);
	}
	static T abs(const matrix& m) throw (exception){
		return abs_ptr(m);
	}
	virtual string to_string() const{
	auto w = this->width(), h = this->height();
	string **ar = new string*[h];
	for (auto i = ar, end = ar + h; i != end; i++) *i = new string[w];
	auto max_len = 0u;
	try{
		for (auto i = 0u; i < h; i++)
			for (auto j = 0u; j < w; j++)
				max_len = std::max((int)(ar[i][j] = My::to_string((*this)[i][j])).length(), (int)max_len);
	}catch(...){
		for (auto i = ar, end = ar + h; i != end; i++) delete[] *i;
		delete[] ar;
		throw;
	}

	char * const spaces = new char[max_len + 1];
	for (auto i = spaces, end = spaces + max_len; i != end; i++){*i = ' ';}
	spaces[max_len] = '\0';

	string res;
	for (auto i = ar, end_i = ar + h; i != end_i; i++){
		string tmp_res;
		for (auto j = *i, end_j = *i + w; j != end_j; j++)
			tmp_res += '|' + *j + string(spaces + j->length());
		res += std::move(tmp_res) + '|' + '\n';
	}

	delete[] spaces;
	for (auto i = ar, end = ar + h; i != end; i++) delete[] *i;
	delete[] ar;

	return res;
}
};

template<class T>
string to_string(const matrix<T>& m){
	return m.to_string();
}

template<class T>
matrix<T> sub_matrix(const matrix<T>& m, const pair<unsigned int, unsigned int>& point, const unsigned int& width, const unsigned int& height) throw(range_error){
	if (point.first + width > m.width() || point.second + height > m.height()) throw (range_error("out of range"));
	matrix<T> res(width, height);
	for (auto i = 0u; i < height; i++){
		for (auto j = 0u; j < width; j++){
			res[i][j] = m[point.first + i][point.second + j];
		}
	}
	return res;
}

template<class T>
//???? flag - true - ?????? ???????, ? ??????? ????????? ???????? ?? ??????????? ????? ? ????????, ??? ? lines ? colums,
//???? flag - false - ?????? ???????, ? ??????? ????????? ???????? ??? ?? ?????? ?? ? ?????? ?? ? ???????, ??? ? lines ? colums,
matrix<T> sub_matrix(const matrix<T>& m, const set<unsigned int>& lines, const set<unsigned int>& columns, const bool& flag_) throw(range_error){
	auto w_m = m.width(), h_m = m.height(), w_res = 0u, h_res = 0u;
	for(const auto& i : lines){
		if (i >= h_m) throw (range_error("out of range"));
	}
	for(const auto& i : columns){
		if (i >= w_m) throw (range_error("out of range"));
	}
	if (flag_){
		w_res = columns.size();
		h_res = lines.size();
	} else {
		w_res = w_m - columns.size();
		h_res = h_m - lines.size();
	}
	matrix<T> res(w_res, h_res);
	auto iter = res.begin();
	if (flag_){
		for (auto i = 0u; i < h_m; i++){
			for (auto j = 0u; j < w_m; j++){
				if (lines.count(i) && columns.count(j))
					*iter++ = m[i][j];
			}
		}
	} else {
		for (auto i = 0u; i < h_m; i++){
			for (auto j = 0u; j < w_m; j++){
				if (!(lines.count(i) || columns.count(j)))
					*iter++ = m[i][j];
			}
		}
	}
	return res;
}

}
