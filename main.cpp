#include <iostream>
#include <chrono>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <functional>
#include <iomanip>
#include <sstream>
#include <cmath>
#include <cassert>

using namespace std;
using namespace std::chrono;

typedef unsigned long long ull;
typedef function<ull(unsigned int)> FibFunc;

struct FibMethod {
	string name;
	FibFunc func;
	double execution_time;
	unsigned int numbers_computed;
};

// BigInt class for arbitrary precision arithmetic
// FIXME: This class does not work. The fibonacci function is not correct & it's likely due to improper operator overloading.
class BigInt {
public:
	BigInt() : sign(true) {}
	BigInt(ull n) : sign(true) {
		while (n > 0) {
			digits.push_back(n % base);
			n /= base;
		}
	}
	BigInt(const string& s) {
		int start = 0;
		if (s[0] == '-') {
			sign = false;
			start = 1;
		}
		for (int i = s.size() - 1; i >= start; i -= digit_count) {
			int num = 0;
			int j = max(start, i - digit_count + 1);
			for (; j <= i; j++) {
				num = num * 10 + (s[j] - '0');
			}
			digits.push_back(num);
		}
	}
	BigInt operator+(const BigInt& other) const {
		BigInt result;
		result.digits.resize(max(digits.size(), other.digits.size()) + 1);
		int carry = 0;
		for (int i = 0; i < result.digits.size(); i++) {
			int sum = carry;
			if (i < digits.size()) sum += digits[i];
			if (i < other.digits.size()) sum += other.digits[i];
			result.digits[i] = sum % base;
			carry = sum / base;
		}
		result.trim();
		return result;
	}
	BigInt operator*(const BigInt& other) const {
		BigInt result;
		result.digits.resize(digits.size() + other.digits.size());
		for (int i = 0; i < digits.size(); i++) {
			int carry = 0;
			for (int j = 0; j < other.digits.size() || carry > 0; j++) {
				long long sum = result.digits[i + j] + carry + (long long)digits[i] * (j < other.digits.size() ? other.digits[j] : 0);
				result.digits[i + j] = sum % base;
				carry = sum / base;
			}
		}
		result.trim();
		return result;
	}
	BigInt operator*(ull n) const {
		BigInt result;
		result.digits.resize(digits.size() + 1);
		ull carry = 0;
		for (int i = 0; i < digits.size() || carry > 0; i++) {
			ull sum = carry + (i < digits.size() ? (ull)digits[i] * n : 0);
			result.digits[i] = sum % base;
			carry = sum / base;
		}
		result.trim();
		return result;
	}
	bool operator==(const BigInt& other) const {
		return sign == other.sign && digits == other.digits;
	}
	BigInt abs() const {
		BigInt result = *this;
		result.sign = true;
		return result;
	}
	bool operator<(const BigInt& other) const {
		if (sign != other.sign) return !sign;
		if (digits.size() != other.digits.size()) return digits.size() < other.digits.size();
		for (int i = digits.size() - 1; i >= 0; i--) {
			if (digits[i] != other.digits[i]) return digits[i] < other.digits[i];
		}
		return false;
	}
	BigInt operator-(const BigInt& other) const {
		if (sign != other.sign) {
			BigInt temp = other;
			temp.sign = sign;
			return *this + temp;
		}
		if (abs() < other.abs()) {
			BigInt result = other - *this;
			result.sign = !result.sign;
			return result;
		}
		BigInt result;
		result.digits.resize(digits.size());
		int carry = 0;
		for (int i = 0; i < result.digits.size(); i++) {
			int diff = carry;
			if (i < digits.size()) diff += digits[i];
			if (i < other.digits.size()) diff -= other.digits[i];
			if (diff < 0) {
				diff += base;
				carry = -1;
			} else {
				carry = 0;
			}
			result.digits[i] = diff;
		}
		result.trim();
		return result;
	}
	void print() const {
		if (!sign) cout << '-';
		cout << digits.back();
		for (int i = digits.size() - 2; i >= 0; i--) {
			cout << setw(digit_count) << setfill('0') << digits[i];
		}
		cout << endl;
	}
	static BigInt fibonacci_fast_doubling(BigInt n) {
		if (n == 0) return 0;
		BigInt a = 0, b = 1;
		for (int i = n.digits.size() * digit_count - 1; i >= 0; i--) {
			BigInt c = a * (b * 2 - a);
			BigInt d = a * a + b * b;
			a = c;
			b = d;
			if (n.digits[i / digit_count] & (1 << (i % digit_count))) {
				BigInt temp = a;
				a = b;
				b = temp + b;
			}
		}
		return a;
	}

private:
	void trim() {
		while (digits.size() > 1 && digits.back() == 0) {
			digits.pop_back();
		}
	}
	static const int base = 1000000000;
	static const int digit_count = 9;
	vector<int> digits;
	bool sign;
};

// -------------------------
// 1. Matrix Exponentiation (O(log n))
// -------------------------
struct Matrix {
	ull mat[2][2];
};

Matrix multiply(Matrix A, Matrix B) {
	Matrix C = {0};
	C.mat[0][0] = A.mat[0][0] * B.mat[0][0] + A.mat[0][1] * B.mat[1][0];
	C.mat[0][1] = A.mat[0][0] * B.mat[0][1] + A.mat[0][1] * B.mat[1][1];
	C.mat[1][0] = A.mat[1][0] * B.mat[0][0] + A.mat[1][1] * B.mat[1][0];
	C.mat[1][1] = A.mat[1][0] * B.mat[0][1] + A.mat[1][1] * B.mat[1][1];
	return C;
}

Matrix power(Matrix M, unsigned int n) {
	Matrix result = {{{1, 0}, {0, 1}}}; // Identity matrix
	while (n > 0) {
		if (n % 2 == 1) result = multiply(result, M);
		M = multiply(M, M);
		n /= 2;
	}
	return result;
}

ull fibonacci_matrix(unsigned int n) {
	if (n == 0) return 0;
	Matrix M = {{{1, 1}, {1, 0}}};
	return power(M, n - 1).mat[0][0];
}

// -------------------------
// 2. Fast Doubling (O(log n))
// -------------------------
pair<ull, ull> fib_doubling(unsigned int n) {
	if (n == 0) return {0, 1};
	auto [a, b] = fib_doubling(n / 2);
	ull c = a * (2 * b - a);
	ull d = a * a + b * b;
	return (n % 2 == 0) ? make_pair(c, d) : make_pair(d, c + d);
}

ull fibonacci_fast_doubling(unsigned int n) {
	return fib_doubling(n).first;
}

// -------------------------
// 3. Iterative Approach (O(n))
// -------------------------
ull fibonacci_iterative(unsigned int n) {
	if (n == 0) return 0;
	if (n == 1) return 1;
	ull a = 0, b = 1;
	for (int i = 2; i <= n; ++i) {
		ull temp = a + b;
		a = b;
		b = temp;
	}
	return b;
}

// -------------------------
// 4. Memoized Iterative DP (O(n))
// -------------------------
vector<ull> fib_cache(1000000, 0); // Increased cache size for bigger numbers

ull fibonacci_memoized(unsigned int n) {
	if (fib_cache[n] != 0) return fib_cache[n];
	fib_cache[0] = 0;
	fib_cache[1] = 1;
	for (int i = 2; i <= n; ++i) {
		fib_cache[i] = fib_cache[i - 1] + fib_cache[i - 2];
	}
	return fib_cache[n];
}

// -------------------------
// Benchmarking Function
// -------------------------
pair<unsigned int, double> count_fibonacci_numbers(FibFunc func) {
	unsigned int count = 0;
	auto start = high_resolution_clock::now();
	auto end_time = start + seconds(1);

	while (high_resolution_clock::now() < end_time) {
		func(count);
		count++;
	}

	auto stop = high_resolution_clock::now();
	double total_time = duration<double, milli>(stop - start).count();

	return {count, total_time};
}

// Function to represent number in scientific notation
string scientific_notation(ull number, int precision = 6) {
	// Handle special cases
	if (isnan(number)) return "NaN";
	if (isinf(number)) return (number > 0 ? "Inf" : "-Inf");
	if (number == 0.0) return "0.0e0";

	ostringstream oss;
	oss << scientific << setprecision(precision) << number;
	return oss.str();
}

// Function to add commas to large numbers
string add_commas(ull number) {
	stringstream ss;
	ss << number;
	string str = ss.str();
	int n = str.size();
	for (int i = n - 3; i > 0; i -= 3) {
		str.insert(i, ",");
	}
	return str;
}

// -------------------------
// Main Function
// -------------------------
int main() {
	// Initialize cache for DP method
	fibonacci_memoized(100000);

	vector<FibMethod> methods = {
		{"Matrix Exponentiation", fibonacci_matrix, 0.0, 0},
		{"Fast Doubling", fibonacci_fast_doubling, 0.0, 0},
		{"Iterative", fibonacci_iterative, 0.0, 0},
		{"Memoized DP", fibonacci_memoized, 0.0, 0}
	};

	// Measure Fibonacci numbers computed within 1 second
	for (auto& method : methods) {
		auto result = count_fibonacci_numbers(method.func);
		method.numbers_computed = result.first;
		method.execution_time = result.second;
	}

	// Sort by most numbers computed (highest to lowest)
	sort(methods.begin(), methods.end(), [](const FibMethod &a, const FibMethod &b) {
		return a.numbers_computed > b.numbers_computed;
	});

	// Print results
	cout << "Fibonacci computation ranking (numbers computed in 1 second):\n";
	for (const auto& method : methods) {
		cout << method.name << ": " << add_commas(method.numbers_computed)
			 << " numbers in " << method.execution_time << " ms\n";
	}

	// Print the largest Fibonacci number computed
	cout << "\nLargest Fibonacci number computed: ";
	cout << "F(" << add_commas(methods[0].numbers_computed - 1) << ") = ";
	cout << scientific_notation(methods[0].func(methods[0].numbers_computed - 1)) << endl;
	cout << fibonacci_fast_doubling(pow(2, 10)) << endl;
	BigInt::fibonacci_fast_doubling(BigInt(pow(2, 10))).print();

	return 0;
}