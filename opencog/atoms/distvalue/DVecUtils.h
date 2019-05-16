/*
 * opencog/atoms/distvalue/DVecUtils.h
 *
 * Copyright (C) 2018 SingularityNet
 * All Rights Reserved
 *
 * Written by Roman Treutlein
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */


#ifndef _OPENCOG_CTUTILS_H
#define _OPENCOG_CTUTILS_H

#include <vector>

namespace opencog
{

/*
 * Utility functions for working with std::vector<double> "DVec" and
 * std::vector<std::vector<double>> "DVecSeq"
 */

typedef std::vector<double> DVec;
typedef std::vector<DVec> DVecSeq;

//Element wise Operators
//returns vector of size min(a.size(),b.size())
DVec operator+(const DVec & a, const DVec & b);
DVec operator-(const DVec & a, const DVec & b);
DVec operator*(const DVec & a, const DVec & b);
DVec operator/(const DVec & a, const DVec & b);

//Multiply or Divide all elements in a with b
DVec operator*(const DVec & a, double b);
DVec operator/(const DVec & a, double b);

//Compares the sum of both Vectors
//Considering a and b as points this coresponds to ordering them
//based on the distance to the origin
bool operator<(const DVec & a, const DVec & b);
bool operator>(const DVec & a, const DVec & b);

//Element wise Comparison
bool operator==(const DVec & a, const DVec & b);

//Sum over all elements in the vector
double sum(const DVec & a);

//Calculate the distance between 2 points
double dist(const DVec & p1, const DVec & p2);

//Calculate the dot product
double dot(const DVec & a, const DVec & b);

//Calculate the mag product
double mag(const DVec & a);

//Calculate cos(angle) between vecotr a and b
double angle(const DVec & a, const DVec & b);

//Given a point p and a Cricle with center c and radius r
//calculate cos(angle) betwen the vector from p to c
//                                  and  from p to a tangent point on the Cricle
double angleTangent(const DVec & p, const DVec & c, double r);

std::string to_string(const DVec& a);
std::string to_string(const DVecSeq& a);

std::ostream& operator<<(std::ostream& os, const DVec &t);
std::ostream& operator<<(std::ostream& os, const DVecSeq &t);

} // namespace opencog
#endif
