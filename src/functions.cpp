#include <RcppArmadillo.h>
#include <functional>
#include <cmath>
#include <limits>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
//' @useDynLib matsunoR, .registration = TRUE
//' @import Rcpp
 

//' Calculate truncated normal cumulative distribution function
//'
//' Computes the cumulative distribution function of a truncated normal
//' distribution in a numerically stable way.
//'
//' Memo: the defualt values of `a` and `b` do not work. The only solution seems to create a wrapper of this using R. 
//' Basically, we cannot set the default values to be Inf. Now, I specify R_NegInf and R_PosInf, but these are not parsed.
//' If I do not specify default values, the function is not compiled. So, this is a hacky way to make it work.
//'
//' @param x Vector of quantiles.
//' @param mean Mean of the normal distribution before truncation (default: 0).
//' @param sd Standard deviation of the normal distribution before truncation (default: 1).
//' @param a Lower truncation point.
//' @param b Upper truncation point.
//'
//' @return Vector of probabilities corresponding to the CDF evaluated at each value in x.
//' @export 
// [[Rcpp::export]]
NumericVector ptruncnorm_cpp(NumericVector x, double mean = 0.0, double sd = 1.0, 
                              double a = R_NegInf, double b = R_PosInf) {
    // Standardize parameters
    NumericVector ux = (x - mean) / sd;
    double ua = (a - mean) / sd;
    double ub = (b - mean) / sd;
    
    // Initialize result vector
    NumericVector res(x.length());
    
    // Set values for points outside bounds
    for(int i = 0; i < x.length(); i++) {
        if(x[i] <= a) {
            res[i] = 0.0;
        } else if(x[i] >= b) {
            res[i] = 1.0;
        }
    }
    
    // Process points between bounds
    for(int i = 0; i < x.length(); i++) {
        if(x[i] > a && x[i] < b) {
            // Calculate CDF values using log transformation for numerical stability
            double logPhi_x = R::pnorm(ux[i], 0.0, 1.0, true, true);
            double logPhi_a = R::pnorm(ua, 0.0, 1.0, true, true);
            
            // Calculate numerator: Φ(x) - Φ(a)
            double num = std::exp(logPhi_x) * (1.0 - std::exp(logPhi_a - logPhi_x));
            
            // Calculate denominator based on truncation type
            double den;
            if(std::isfinite(b) && std::isfinite(a)) {
                // Two-sided truncation: denominator = Φ(b) - Φ(a)
                double logPhi_b = R::pnorm(ub, 0.0, 1.0, true, true);
                den = std::exp(logPhi_b) * (1.0 - std::exp(logPhi_a - logPhi_b));
            } else if(std::isfinite(b) && !std::isfinite(a)) {
                // Lower bound at -Inf: denominator = Φ(b) - 0
                double logPhi_b = R::pnorm(ub, 0.0, 1.0, true, true);
                den = std::exp(logPhi_b);
            } else {
                // Upper bound at Inf: denominator = 1 - Φ(a)
                den = std::exp(R::pnorm(ua, 0.0, 1.0, false, true));
            }
            
            if(den == 0.0) {
                Rcpp::warning("Division by zero encountered in ptruncnorm_cpp. Setting result to 0.");
                res[i] = 0.0;
            } else {
                res[i] = num / den;
            }
        }
    }
    
    return res;
}