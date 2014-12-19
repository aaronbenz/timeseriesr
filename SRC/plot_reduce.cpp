//
//  plot_reduce.cpp
//  
//
//  Created by Aaron Benz on 10/16/14.
//
//
#include <Rcpp.h>
#include <algorithm>
//#include "plot_reduce.h"
using namespace Rcpp;
//' Reduce the number of points needed to plot time series data
//' @description A very fast way to reduce the number of points in a time-series dataset. This algorithm takes a tolerance
//' range and will return all of the critical points/indexes over that range
//' 
//' @param x A numeric vector
//' @param tolerance A decimal representing the percentage of tolerance acceptible
//' 
//' @return The value return is a list of two vectors:
//' index | vector contained indexed values of critical points
//' values | vector containing the actual values
//' 
//' @examples
//' point_reduce(1001:1100, .1)
//' 
//' #takes a sin curve of 99001 in length and reduces it down to 3008 points
//' point_reduce(sin(seq(1,100,.001)),.01)
// [[Rcpp::export]]
List point_reduce(NumericVector x, double tolerance){
    NumericVector::iterator it, x_it;
    std::vector<int> out;
    double tmp;   
    double tol = (max(x) - min(x)) * tolerance;
    
    x_it = x.begin();
    out.push_back(1);
    it = x_it;
    for (int i = 0; it < x.end(); ++it, i++) {
        tmp = *x_it - *it;
        if(tmp < 0){
            tmp = tmp * -1;
        } 
        if(tmp > tol){ //#bug with abs() when numbers are less than 1, using comparitor above instead
            ++x_it;
            *x_it = *it;
            out.push_back(i+1);
        }
    }
    
    if(*x_it != x(x.size()-1)){
      ++x_it;
      *x_it = x(x.size()-1);
      out.push_back(x.size());
    }
      ++x_it; //want everything between begining and x_it
      NumericVector out_val(std::distance(x.begin(),x_it));
      NumericVector::iterator out_it;
      out_it = out_val.begin();
      it = x.begin();
      while(it < x_it){
        *out_it = *it;
        ++out_it;
        ++it;
      }
      return List::create(_["index"] = out, _["values"] = out_val);
}

//' De-duplicates a numeric vector/data.table/data.frame
//' @description These set of functions take a vector, or a datatable/dataframe (with a particular column)
//' and compress it by de-duplicating its values. Ideally, this is a good way to reduce the amount of data
//' for time-series use cases
//' 
//' @param x A vector/data.table/data.frame
//' @param column If using a data.table/data.frame, the index for performing the operation on
//' 
//' @return Returns a vector/data.table/data.frame that has been de-deduplicated. 
//' De-duplicated means that consecutive duplicate values are reduced to the first occurance. 
//' 
//' @examples
//' vec <- c(1,1,2,2,3,3,4,4,5,5,4,4,3,3,2,2,1,1)
//' vdeduplicate(vec)
// [[Rcpp::export]]
NumericVector vdeduplicate(NumericVector x){
      NumericVector::iterator it, out_it, i;
      it = std::unique (x.begin(), x.end());
      NumericVector out(std::distance(x.begin(),it));
      
      //Loops through using iterators
      //Because the values that we want are at x[0] (aka *x.begin()) and go to where 'it' left off
      //We just need to get the value of all the pointers between those to pointers
      for(i = x.begin(),out_it = out.begin(); i != it; ++i, ++out_it){
        *out_it = *i;
      }
      return out;      
}

