#include <RcppArmadillo.h>
#include <functional>
#include <cmath>
#include <limits>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
//' @useDynLib matsunoR
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

//' Simple numerical integration using trapezoidal rule
//'
//' High-performance C++ implementation using simple trapezoidal rule.
//' Fast and straightforward integration for smooth functions.
//'
//' @param f R function to integrate
//' @param lower Lower bound of integration
//' @param upper Upper bound of integration
//' @param n Number of subdivisions (default: 1000)
//'
//' @return List containing:
//' \\itemize{
//'   \\item value: The integral value
//'   \\item subdivisions: Number of subdivisions used
//' }
//' @export
// [[Rcpp::export]]
List integrate_cpp(Function f, double lower, double upper, int n = 1000) {
    
    // Handle infinite bounds
    if(!std::isfinite(lower) || !std::isfinite(upper)) {
        stop("Infinite bounds not supported in current implementation");
    }
    
    if(lower >= upper) {
        stop("Lower bound must be less than upper bound");
    }
    
    if(n <= 0) {
        stop("Number of subdivisions must be positive");
    }
    
    // Calculate step size
    double dx = (upper - lower) / n;
    
    // Create x values
    NumericVector x_vals(n + 1);
    for(int i = 0; i <= n; i++) {
        x_vals[i] = lower + i * dx;
    }
    
    // Evaluate function at all points
    NumericVector f_vals = as<NumericVector>(f(x_vals));
    
    // Check for NA values
    for(int i = 0; i < f_vals.length(); i++) {
        if(NumericVector::is_na(f_vals[i])) {
            stop("Function evaluation returned NA during integration");
        }
    }
    
    // Apply trapezoidal rule: sum of (f(i) + f(i+1)) * dx / 2
    double integral = 0.0;
    for(int i = 0; i < n; i++) {
        integral += dx * (f_vals[i] + f_vals[i + 1]) / 2.0;
    }
    
    return List::create(
        _["value"] = integral,
        _["subdivisions"] = n
    );
}