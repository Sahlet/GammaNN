//string.h
#pragma once
#ifndef _TO_STRING
#define _TO_STRING
#include <string>
namespace My{
	struct dub_to_str{
		static std::string duble_to_string(const long double& val){
			char str[50];
			sprintf_s(str, "%.30lf", val);
			auto res = std::string(str);
			auto i = res.rbegin();
			unsigned int count = 0;
			while (*i++ == '0'){ count++; }
			if (res[res.length() - count - 1] == ',' || res[res.length() - count - 1] == '.') count--;
			return res.substr(0, res.length() - count);
		}
	};
	
	inline std::string to_string(const long double& val){
		return dub_to_str::duble_to_string(val);
	}
	inline std::string to_string(const double& val){ return to_string((long double)val); }
	inline std::string to_string(const float& val){ return to_string((long double)val); }
	inline std::string to_string(const std::string& val){ return val; }

	template<class T>
	inline std::string to_string(const T& val){
		return std::to_string(val);
	}
}
#endif