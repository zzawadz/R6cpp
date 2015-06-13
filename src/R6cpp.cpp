#include "R6cpp.h"

using namespace Rcpp;

R6Test::R6Test(int size)
{
  _size = size;
  _vec.reserve(size);
}

void R6Test::push_back(const double& val)
{
  _vec.push_back(val);
}

double R6Test::at(const int& pos)
{
  return _vec.at(pos);
}
