//
//  plot_reduce.cpp
//  
//
//  Created by Aaron Benz on 10/16/14.
//
//
#include <Rcpp.h>
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
    std::vector<int> index;
    std::vector<double> values;
    std::vector<double> wtf;
    double tmp = .0001;   
    double tol = (max(x) - min(x)) * tolerance;

    index.push_back(1);//take first value in the vector always
    values.push_back(x[0]);
    
    for (int i = 1; i < x.size(); i++) {
        tmp = values.back() - x[i];
        if(tmp < 0){
            tmp = tmp * -1;
        } 
        if(tmp > tol){ //#bug with abs() when numbers are less than 1, using comparitor above instead
            index.push_back(i+1);
            values.push_back(x[i]);
        }else{
            wtf.push_back(abs(values.back() - x[i]));
        }
    }
    
    
    //if last value is not there, add it as we want to maintain endpoints always
    if(index.back() != x.size()){
        index.push_back(x.size());
        values.push_back(x[x.size() - 1]);
    }

    return List::create(_["index"] = index, _["values"] = values);
}


