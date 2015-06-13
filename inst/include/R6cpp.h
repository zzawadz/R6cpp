#include <Rcpp.h>
#include <vector>

//[[R6Begin]]
class R6Test
{
  public:

  int _size;
  std::vector<double> _vec;

  R6Test(int size);
  void push_back(const double& val);
  double at(const int& pos);

};
//[[R6End]]


