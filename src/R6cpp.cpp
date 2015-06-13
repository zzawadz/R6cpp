#include "R6cpp.h"

using namespace Rcpp;

R6Test::R6Test(int size)
{
  _size = size;
}

void R6Test::push_back(double val)
{
  _vec.push_back(val);
}

double R6Test::at(int pos)
{
  return _vec.at(pos);
}
