#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

//[[R6Begin]]
class TestVector
{
  std::vector<double> vec;


public:

  //[[R6:vv=xptr]]
  TestVector(const double a1, int a2, const std::vector<double>& vv);
  void push_back(double val, const double& val2);
  double at(int idx);
  const double fnc_test(std::vector<double>& vec);

  double testValue; //[[R6: get, set]]
  //[[R6: get, set]]
  double testVal2;
  double val;
  const double cnst;
};
//[[R6End]]

//[[R6Begin]]
class TestRow
{
  TestVector row;


public:

  bool isChecked;

  TestRow();
  void push_back(double val);
  double at(int idx);
  TestVector as_vector();

};
//[[R6End]]

