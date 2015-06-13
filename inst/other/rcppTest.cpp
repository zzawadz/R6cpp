#include <Rcpp.h>
#include <vector>
using namespace Rcpp;



// [[Rcpp::export]]
void test_cpp(NumericVector& x) {
  x[0] = 100;
}




/*** R

*/
