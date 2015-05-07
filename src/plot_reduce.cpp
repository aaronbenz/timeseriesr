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
//' @param vec A numeric vector
//' @param tolerance A decimal representing the percentage of tolerance acceptible
//' 
//' @return The value returned is a vector consisting of the indexed values that should be kept
//' 
//' @examples
//' vreduce(1001:1100)
//' vreduce(1001:1100, .1)
//' 
//' #takes a sin curve of 10000 in length and reduces it down to 2447 points
//' vreduce(sin(seq(1,100,length.out = 10000)))
// [[Rcpp::export]]
NumericVector vreduce(NumericVector vec, double tolerance = .01){
    //it, whatever it is currently pointing at in a loop, aka the iterator
    //x_it, the last value in x that we want to keep. So it gets updated when there is a new unique value given by it
    NumericVector x = clone(vec);
    NumericVector::iterator it, x_it;
    std::vector<int> out; //used to keep the index values
    double tmp;   
    double tol = (max(x) - min(x)) * tolerance;
    
    //use pointers to iterate through vector
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
    //if the last value of the original vector is not in x_it, put it in there
    if(*x_it != x(x.size()-1)){
      ++x_it;
      *x_it = x(x.size()-1);
      out.push_back(x.size());
    }
      ++x_it; //want everything between beginning and x_it
      NumericVector out_val(std::distance(x.begin(),x_it));
      NumericVector::iterator out_it;
      out_it = out_val.begin();
      it = x.begin();
      while(it < x_it){
        *out_it = *it;
        ++out_it;
        ++it;
      }
      //return List::create(_["index"] = out, _["values"] = out_val);
      return wrap(out);
}

//This tempate class follows the same logic as std::unique, except instead of getting the values, it
//gets the indexes of the values
template <class ForwardIterator> ForwardIterator uniqueIndex (ForwardIterator first, ForwardIterator last)
{
  if (first==last) return last;
  int i = 1;
  int current = 1;
  
  ForwardIterator result = first;
  
  while (first != last){
    //Rcout << *result << "---" << *first << std::endl;
    if (!(*result == *first)){ // or: if (!pred(*result,*first)) for version (2)
      *result = current;
      current = i;
      ++result;
      *result = *first;
    }
    i++;
    ++first;
  }
  //add last current value and i, which is the last value in the entire set
  if(*result != current){
    *result = current;
  }
  if(current != i) {
    ++result;
    *result = i;
  }
  return result;
}

//' De-duplicates a numeric vector/data.table/data.frame
//' @description These set of functions take a vector, or a datatable/dataframe (with a particular column)
//' and compress it by de-duplicating its values. Ideally, this is a good way to reduce the amount of data
//' for time-series use cases
//' 
//' @param x A vector/data.table/data.frame
//' @param returnIndex If TRUE, will return indexes, otherwise will return actual values
//' @param key If using a data.table/data.frame, the index for performing the operation on
//' 
//' @return Returns a vector/data.table/data.frame that has been de-deduplicated. 
//' De-duplicated means that consecutive duplicate values are reduced to the first occurance. 
//' 
//' @examples
//' vec <- c(1,1,2,2,3,3,4,4,5,5,4,4,3,3,2,2,1,1)
//' vdeduplicate(vec)
//' vdeduplicate(vec, TRUE)
// [[Rcpp::export]]
NumericVector vdeduplicate(NumericVector x, bool returnIndex = false){
      NumericVector::iterator it, out_it, i;
      NumericVector xx = clone(x);
      if(returnIndex){
              it = uniqueIndex(xx.begin(), xx.end()); //cloning object so it does not change the actual R object
      }else{
              it = std::unique (xx.begin(), xx.end());
      }
      NumericVector out(std::distance(xx.begin(),it));
      
      //Loops through using iterators
      //Because the values that we want are at x[0] (aka *x.begin()) and go to where 'it' left off
      //We just need to get the value of all the pointers between those to pointers
      for(i = xx.begin(),out_it = out.begin(); i != it; ++i, ++out_it){
        *out_it = *i;
      }
      return out;      
}


