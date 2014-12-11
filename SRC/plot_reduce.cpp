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

//a very fast way to see all of the critical points of a time-series type problem
// [[Rcpp::export]]
List reducePoints(NumericVector x, double tolerance){
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


