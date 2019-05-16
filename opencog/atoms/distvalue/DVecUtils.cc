#include <algorithm>
#include <cmath>
#include <sstream>
#include <ostream>

#include <opencog/util/exceptions.h>
#include <opencog/util/numeric.h>

#include "DVecUtils.h"

namespace opencog
{

DVec operator+(const DVec& a, const DVec& b)
{
    DVec result;

    const std::size_t n = std::min(a.size(), b.size()) ;
    std::transform(std::begin(a), std::begin(a)+n, std::begin(b),
                   std::back_inserter(result), std::plus<double>{});
    return result;
}

DVec operator-(const DVec& a, const DVec& b)
{
    DVec result;

    const std::size_t n = std::min(a.size(), b.size()) ;
    std::transform(std::begin(a), std::begin(a)+n, std::begin(b),
                   std::back_inserter(result), std::minus<double>{});
    return result;
}

DVec operator*(const DVec& a, const DVec& b)
{
    DVec result;

    const std::size_t n = std::min(a.size(), b.size()) ;
    std::transform(std::begin(a), std::begin(a)+n, std::begin(b),
                   std::back_inserter(result), std::multiplies<double>{});
    return result;
}

DVec operator/(const DVec& a, const DVec& b)
{
    DVec result;

    const std::size_t n = std::min(a.size(), b.size()) ;
    std::transform(std::begin(a), std::begin(a)+n, std::begin(b),
                   std::back_inserter(result), std::divides<double>{});
    return result;
}

DVec operator*(const DVec & a, double b)
{
    DVec result;

	for (double elem : a)
		result.push_back(elem * b);

    return result;
}

DVec operator/(const DVec & a, double b)
{
    DVec result;

	for (double elem : a)
		result.push_back(elem / b);

    return result;
}

bool operator<(const DVec & a, const DVec & b)
{
	return sum(a) < sum(b);
}

bool operator>(const DVec & a, const DVec & b)
{
	return sum(a) > sum(b);
}

bool operator==(const DVec & a, const DVec & b)
{
	if (a.size() != b.size())
		return false;
	for (unsigned int i = 0; i < a.size(); i++)
	{
		if (!is_approx_eq_ulp(a[i],b[i],24))
			return false;
	}
	return true;
}

double sum(const DVec & a)
{
	double res = 0;
	for (double e : a)
		res += e;
	return res;
}

double dist(const DVec & p1, const DVec & p2)
{
	double dist = 0;
    for(size_t i = 0; i < p1.size(); i++)
    {
        double d = p1[i] - p2[i];
        dist += d * d;
    }
    return sqrt(dist);
}

double dot(const DVec & a, const DVec & b)
{
	if (a.size() != b.size())
		throw RuntimeException(TRACE_INFO,"Vectors must be the same lenght.");
	double sum = 0;
	for (size_t i = 0; i < a.size(); i++)
	{
		sum += a[i] * b[i];
	}
	return sum;
}

double mag(const DVec & a)
{
	double dist = 0;
    for(size_t i = 0; i < a.size(); i++)
        dist += a[i] * a[i];
    return sqrt(dist);
}

double angle(const DVec & a, const DVec & b)
{
	if (a.size() == 1)
	{
		if (a[0] * b[0] < 0.0)
			return 0;
		else
			return 1;
	}
	return dot(a,b) / mag(a) * mag(b);
}

double angleTangent(const DVec & p, const DVec & c, double r)
{
	double d = dist(p,c);
	double l = sqrt(d*d + r*r);

	return l / r;
}

std::string to_string(const DVec & a)
{
	if (a.size() == 0)
		return "[]";
	std::stringstream ss;
	ss << '[';
	for (double d : a)
	{
		ss << d << ",";
	}
	ss << "\b]";
	return ss.str();
}

std::string to_string(const DVecSeq & a)
{
	if (a.size() == 0)
		return "[]";
	std::stringstream ss;
	ss << '[';
	for (DVec d : a)
	{
		ss << to_string(d) << ",";
	}
	ss << "\b]";
	return ss.str();
}

std::ostream& operator<<(std::ostream & os, const DVec & t)
{
	os << to_string(t);
	return os;
}

std::ostream& operator<<(std::ostream & os, const DVecSeq & t)
{
	os << to_string(t);
	return os;
}

} // namespace opencog
