/* *****************************************************************************
 *                                                                             *   
 * Author: Laurent R. Berge                                                    *
 * Purpose: Custom EM algorithm                                                *
 *                                                                             *
 ******************************************************************************/

#include <Rcpp.h>
#include <math.h>
#include <Rmath.h>
using namespace Rcpp;

// Functions for the estimation of EM

//
// Setting the parameters => no need to copy them every time we use them
//

struct X_parameters{
    bool is_set = false;
    NumericMatrix X;
    IntegerVector n_cases;
    int n;
    int K; // number of variables
};

static X_parameters X_prms;

// [[Rcpp::export]]
void cpp_setup_X_parameters(NumericMatrix X, StringVector method){
    
    int K = X.ncol();
    int n = X.nrow();
    
    X_prms.n = n;
    X_prms.K = K;
    X_prms.X = X;
    
    IntegerVector n_cases(K);
    
    for(int k=0 ; k<K ; ++k){
        if(method[k] == "multinomial"){
            // The mutlinomial vector must range btw 0 and n_case - 1
            int m = 0;
            for(int i=0 ; i<n ; ++i){
                if(m < X(i, k)){
                    m = X(i, k);
                }
            }
            // saving it
            n_cases[k] = m + 1;
        }
    }
    
    X_prms.n_cases = n_cases;
    
    X_prms.is_set = true;
    
}

// [[Rcpp::export]]
List cpp_get_X_parameters(){
    
    if(X_prms.is_set == false){
        stop("X hasn't been set!");
    }
    
    List res;
    
    res["n"] = X_prms.n;
    res["k"] = X_prms.K;
    res["X"] = X_prms.X;
    res["n_cases"] = X_prms.n_cases;
    
    return res;
}

//
// Setting tau => no need to copy it and compute the same statistics every time we use it
//

struct tau_parameters{
    bool is_set = false;
    NumericMatrix tau;
    NumericVector sum_tau;
    int n;
    int M; // number of categories
};

static tau_parameters tau_prms;

// [[Rcpp::export]]
void cpp_setup_tau_parameters(NumericMatrix tau){
    
    int M = tau.ncol();
    int n = tau.nrow();
    NumericVector sum_tau(M);
    
    for(int m=0 ; m<M ; ++m){
        double s = 0;
        for(int i=0 ; i<n ; ++i){
            s += tau(i, m);
        }
        sum_tau[m] = s;
    }
    
    // setting it
    tau_prms.M = M;
    tau_prms.n = n;
    tau_prms.tau = tau;
    tau_prms.sum_tau = sum_tau;
    tau_prms.is_set = true;
    
}

// [[Rcpp::export]]
List cpp_get_tau_parameters(){
    
    if(X_prms.is_set == false){
        stop("tau hasn't been set!");
    }
    
    List res;
    
    res["n"] = tau_prms.n;
    res["M"] = tau_prms.M;
    res["tau"] = tau_prms.tau;
    res["sum_tau"] = tau_prms.sum_tau;
    
    return res;
}

// [[Rcpp::export]]
NumericMatrix cpp_get_tau(){
    
    if(tau_prms.is_set == false){
        stop("tau hasn't been set!");
    }
    
    return tau_prms.tau;
}

// [[Rcpp::export]]
void cpp_cleanup(){
    
    // cleans tau and X
    X_prms.is_set   = false;
    tau_prms.is_set = false;
    
    NumericMatrix X(1,1);
    NumericMatrix tau(1,1);
    
    X_prms.X = X;
    tau_prms.tau = tau;
    
}

// [[Rcpp::export]]
double cpp_map(NumericMatrix x){
    // Computes tau + set it in static
    // returns the likelihood
    // x: the "numerator" ; N x M ; equals to ll_im
    // M: number of different categories
    
    int n = x.nrow();
    int M = x.ncol();
    
    // computing the MAP: getting the max + computing tau
    NumericMatrix tau(n, M);
    
    double x_tmp;
    for(int i=0 ; i<n ; ++i){
        
        // we do it that way for numerical stability
        double XMAX = x(i, 0);
        for(int m=1 ; m<M ; ++m){
            if(XMAX < x(i, m)) XMAX = x(i, m);
        }
        
        double x_sum = 0;
        for(int m=0 ; m<M ; ++m){
            x_tmp = exp(x(i, m) - XMAX);
            tau(i, m) = x_tmp;
            x_sum += x_tmp;
        }
        
        for(int m=0 ; m<M ; ++m){
            tau(i, m) /= x_sum;
        }
    }
    
    // Computing the log likelihood + setting the new tau values
    double ll = 0;
    
    // we set the new tau
    NumericVector sum_tau(M);
    
    double tau_tmp;
    for(int m=0 ; m<M ; ++m){
        double s = 0;
        for(int i=0 ; i<n ; ++i){
            tau_tmp = tau(i, m);
            s += tau_tmp;
            ll += tau_tmp * x(i, m);
        }
        sum_tau[m] = s;
    }
    tau_prms.tau = tau;
    tau_prms.sum_tau = sum_tau;
    
    return ll;
}

NumericMatrix multinom_params(int k, bool final = false){
    
    // retrieving the information on tau and x
    NumericMatrix &tau = tau_prms.tau;
    NumericMatrix &X = X_prms.X;
    
    int n = X_prms.n;
    int M = tau_prms.M;
    int n_cases = X_prms.n_cases[k];
    
    // Step 1: finding the parameters
    NumericMatrix prms(n_cases, M); 
    
    for(int m=0 ; m<M ; ++m){
        for(int i=0 ; i<n ; ++i){
            prms(static_cast<int>(X(i, k)), m) += tau(i, m);
        }
    }
    
    // normalizing
    for(int m=0 ; m<M ; ++m){
        double sum_case = 0;
        for(int c=0 ; c<n_cases ; ++c){
            sum_case += prms(c, m);
        }
        
        // Rcout << "sum_case: " << sum_case << "\n";
        
        double lsum_case = log(sum_case);
        for(int c=0 ; c<n_cases ; ++c){
            if(prms(c, m) == 0){
                prms(c, m) = -30;
            } else {
                prms(c, m) = log(prms(c, m)) - lsum_case;
            }
        }
        
        if(final){
            // we express in probability (not in log)
            for(int c=0 ; c<n_cases ; ++c){
                prms(c, m) = exp(prms(c, m));
            }
        }
    }
    
    return prms;
}

NumericMatrix multinom_contribution(int k){
    
    // retrieving the information
    NumericMatrix &X = X_prms.X;
    
    int n = X_prms.n;
    int M = tau_prms.M;
    
    // Step 1: finding the parameters
    NumericMatrix prms = multinom_params(k); 
    
    // STEP 2: contribution to the LL
    NumericMatrix ll(n, M);
    for(int m=0 ; m<M ; ++m){
        for(int i=0 ; i<n ; ++i){
            ll(i, m) = prms(static_cast<int>(X(i, k)), m);
        }
    }
    
    return ll;
}

// [[Rcpp::export]]
List cpp_m_multinomial(int k_R, int breakpoint){
    // tau is always a N x 2 matrix
    // x MUST be an integer vector from 0 to n_cases - 1

    if(X_prms.is_set == false){
        stop("tau hasn't been set!");
    }

    if(tau_prms.is_set == false){
        stop("tau hasn't been set!");
    }

    int k = k_R - 1;

    // retrieving the information
    NumericMatrix &tau = tau_prms.tau;

    NumericMatrix &X = X_prms.X;

    int n = X_prms.n;
    int M = tau_prms.M;
    int n_cases = X_prms.n_cases[k];

    Rcout << "n = " << n << "; M = " << M << "; n_cases = " << n_cases << "\n";

    if(breakpoint == 0) stop("setup");

    // Step 1: finding the parameters
    NumericMatrix prms(n_cases, M); 
    // we start at 0.001, so there's no need to normalize (useful when taking logs later)
    for(int m=0 ; m<M ; ++m){
        for(int c=0 ; c<n_cases ; ++c){
            prms(c, m) = 0.001;
        }
    }


    if(breakpoint == 3){
        List res;
        res["prms"] = prms;
        return res;
    }

    int index = 0;
    for(int m=0 ; m<M ; ++m){
        for(int i=0 ; i<n ; ++i){
            index = static_cast<int>(X(i, k));
            if(index >= n_cases || index < 0){
                Rcout << "index: " << index << "\n";
                stop("index problem");
            }
            prms(index, m) += tau(i, m);
        }
    }

    if(breakpoint == 1) stop("filling");

    // normalizing
    for(int m=0 ; m<M ; ++m){
        double sum_case = 0;
        for(int c=0 ; c<n_cases ; ++c){
            sum_case += prms(c, m);
        }
        
        // Rcout << "sum_case: " << sum_case << "\n";

        double lsum_case = log(sum_case);
        for(int c=0 ; c<n_cases ; ++c){
            prms(c, m) = log(prms(c, m)) - lsum_case;
        }
    }


    if(breakpoint == 2) stop("normalizing");

    // STEP 2: contribution to the LL
    NumericMatrix ll(n, M);
    for(int m=0 ; m<M ; ++m){
        for(int i=0 ; i<n ; ++i){
            ll(i, m) = prms(static_cast<int>(X(i, k)), m);
        }
    }

    List res;
    res["prms"] = prms;
    res["ll"] = ll;

    return res;
}

NumericMatrix normal_params(int k){
    
    // retrieving the information
    NumericMatrix &tau = tau_prms.tau;
    NumericVector &sum_tau = tau_prms.sum_tau;
    
    NumericMatrix &X = X_prms.X;
    int n = X_prms.n;
    int M = tau_prms.M;
    
    
    NumericMatrix prms(2, M);
    // The second row is not the standard-error but the variance
    
    for(int m=0 ; m<M ; ++m){
        double x_tmp;
        double x_tau;
        for(int i=0 ; i<n ; ++i){
            x_tmp = X(i, k);
            x_tau = x_tmp * tau(i, m);
            prms(0, m) += x_tau;
            prms(1, m) += x_tau * x_tmp;
        }
    }
    
    for(int m=0 ; m<M ; ++m){
        prms(0, m) /= sum_tau[m];
        prms(1, m) /= sum_tau[m];
        prms(1, m) -= prms(0, m) * prms(0, m);
        prms(1, m) = sqrt(prms(1, m));
    }
    
    return prms;
}

NumericMatrix normal_contribution(int k){
    
    // retrieving the information
    
    NumericMatrix &X = X_prms.X;
    int n = X_prms.n;
    int M = tau_prms.M;
    
    // STEP 1: Computing the parameters
    
    NumericMatrix prms = normal_params(k);
    
    // STEP 2: contribution to the LL
    
    NumericMatrix ll(n, M);
    for(int m=0 ; m<M ; ++m){
        double sigma = prms(1, m);
        double constant = -0.9189385 - log(sigma);
        double mu = prms(0, m);
        double inv_sigma = 1 / (2 * sigma * sigma);
        double x_mu;
        for(int i=0 ; i<n ; ++i){
            x_mu = X(i,k) - mu;
            ll(i, m) = constant - inv_sigma * x_mu * x_mu;
        }
    }
    
    return ll;
}


// [[Rcpp::export]]
List cpp_m_normal(int k_R){
    // computes the parameters and the contribution to the LL
    // the values of X and tau are already loaded

    int k = k_R - 1;

    // retrieving the information
    NumericMatrix &tau = tau_prms.tau;
    NumericVector &sum_tau = tau_prms.sum_tau;

    NumericMatrix &X = X_prms.X;
    int n = X_prms.n;
    int M = tau_prms.M;

    // STEP 1: Computing the parameters

    NumericMatrix prms(2, M);
    // The second row is not the standard-error but the variance

    for(int m=0 ; m<M ; ++m){
        double x_tmp;
        double x_tau;
        for(int i=0 ; i<n ; ++i){
            x_tmp = X(i, k);
            x_tau = x_tmp * tau(i, m);
            prms(0, m) += x_tau;
            prms(1, m) += x_tau * x_tmp;
        }
    }

    for(int m=0 ; m<M ; ++m){
        prms(0, m) /= sum_tau[m];
        prms(1, m) /= sum_tau[m];
        prms(1, m) -= prms(0, m) * prms(0, m);
    }

    // STEP 2: contribution to the LL

    // NOTA: we don't care about the constant (1/sqrt(2*pi)), we only compute the varying LL contribution
    // because the constant will be washed away in the MAP (ie p1 / (p1 + p2 + p3))
    NumericMatrix ll(n, M);
    for(int m=0 ; m<M ; ++m){
        double sigma_2 = prms(1, m);
        double ln_sigma = 1/2 * log(sigma_2);
        double mu = prms(0, m);
        double inv_sigma = 1 / (2 * sigma_2);
        double x_mu;
        for(int i=0 ; i<n ; ++i){
            x_mu = X(i,k) - mu;
            ll(i, m) = -ln_sigma - inv_sigma * x_mu * x_mu;
        }
    }

    List res;
    res["prms"] = prms;
    res["ll"] = ll;

    return res;

}

inline double maxDiff(double x, double y){
    double x_abs = fabs(x);
    double y_abs = fabs(y);
    return x_abs > y_abs ? x_abs : y_abs;
}

NumericMatrix NR_beta(NumericVector ln_x, NumericVector ln_1_x, NumericVector s_mean, NumericVector s_var){

    double eps = 0.00001;
    int iterMax = 50;

    int M = ln_x.length();

    NumericMatrix prms(2, M);

    // We loop over m
    for(int m=0 ; m<M ; ++m){

        double alpha_old, alpha;
        double beta_old, beta;

        // Starting values => method of moments
        double tmp = (s_mean[m] * (1 - s_mean[m])) / s_var[m] - 1;
        alpha_old = s_mean[m] * tmp;
        beta_old  = (1 - s_mean[m]) * tmp;
        
        // Rcout << "\n\nln_x: " << ln_x[m] << " ln_1_x: " << ln_1_x[m] << " mean: "<< s_mean[m] << " var: " << s_var[m] << "\n";

        // Rcout << "m: " << m << "\nBEGIN: alpha: " << alpha_old << " beta: " << beta_old << "\n";

        int iter = 0;

        double di_ab = R::digamma(alpha_old + beta_old);
        double tri_ab;
        double g1 = ln_x[m] + di_ab - R::digamma(alpha_old);
        double g2 = ln_1_x[m] + di_ab - R::digamma(beta_old);
        double diff = maxDiff(g1, g2);

        double G11, G22, inv_det;

        while(diff > eps && iter++ < iterMax){
            
            // Rcout << "\niter: " << iter << "\n";
            
            // Rcout << "g1: " << g1 << " g2: " << g2 << "\n";

            // Computing the update
            tri_ab = R::trigamma(alpha_old + beta_old);
            G11 = tri_ab - R::trigamma(alpha_old);
            G22 = tri_ab - R::trigamma(beta_old);

            inv_det = 1 / (G11 * G22 - tri_ab * tri_ab);
            
            // Rcout << "G11: " << G11 << " G22: " << G22 << " tri_ab: " << tri_ab << " inv_det: " << inv_det << "\n";

            alpha = alpha_old - inv_det * (G22 * g1 - tri_ab * g2);
            beta  =  beta_old - inv_det * (- tri_ab * g1 + G11 * g2);
            
            if(alpha < 0){
                alpha = alpha_old / 2;
                // Rcout << "alpha/2\n";
            }
            if(beta < 0){
                beta = beta_old / 2;
                // Rcout << "beta/2\n";
            }
            
            // Rcout << "alpha: " << alpha << " beta: " << beta << "\n";

            di_ab = R::digamma(alpha + beta);
            g1 = ln_x[m] + di_ab - R::digamma(alpha);
            g2 = ln_1_x[m] + di_ab - R::digamma(beta);

            alpha_old = alpha;
            beta_old  = beta;

            diff = maxDiff(g1, g2);
        }

        if(iter == iterMax) stop("Maximum iterations reached.");

        // Rcout << "  END: alpha: " << alpha_old << " beta: " << beta_old << "\n";

        prms(0, m) = alpha_old;
        prms(1, m) = beta_old;
    }

    return prms;
}

NumericMatrix beta_params(int k){
    
    // retrieving the information on tau and X
    NumericMatrix &tau = tau_prms.tau;
    NumericVector &sum_tau = tau_prms.sum_tau;
    NumericMatrix &X = X_prms.X;
    int n = X_prms.n;
    int M = tau_prms.M;
    
    // We need some statistics to perform the NR algorithm
    NumericVector ln_x(M);
    NumericVector ln_1_x(M);
    NumericVector s_mean(M);
    NumericVector s_var(M);
    
    for(int m=0 ; m<M ; ++m){
        double x_tmp;
        double tau_tmp;
        for(int i=0 ; i<n ; ++i){
            x_tmp = X(i, k);
            tau_tmp = tau(i, m);
            
            ln_x(m)   += tau_tmp * log(x_tmp);
            ln_1_x(m) += tau_tmp * log(1 - x_tmp);
            tau_tmp *= x_tmp;
            s_mean(m) += tau_tmp;
            s_var(m)  += tau_tmp * x_tmp;
        }
    }
    
    // Normalization
    for(int m=0 ; m<M ; ++m){
        ln_x(m)   /= sum_tau[m];
        ln_1_x(m) /= sum_tau[m];
        
        s_mean(m) /= sum_tau[m];
        s_var(m)  /= sum_tau[m];
        
        s_var(m)  -= s_mean(m) * s_mean(m);
    }
    
    // Now the NR algorithm
    NumericMatrix prms = NR_beta(ln_x, ln_1_x, s_mean, s_var);
    
    return prms;
}

NumericMatrix beta_contribution(int k){
    
    // retrieving the information on tau and X
    NumericMatrix &X = X_prms.X;
    int n = X_prms.n;
    int M = tau_prms.M;
    
    // STEP 1: estimation of the parameters
    
    NumericMatrix prms = beta_params(k);
    
    // STEP 2: contribution
    
    NumericMatrix ll(n, M);
    
    for(int m=0 ; m<M ; ++m){
        double x_tmp;
        double alpha = prms(0, m);
        double beta  = prms(1, m);
        double constant = lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta);
        
        for(int i=0 ; i<n ; ++i){
            x_tmp = X(i, k);
            ll(i, m) = (alpha + 1) * log(x_tmp) + (beta + 1) * log(1 - x_tmp) + constant;
        }
    }
    
    return ll;
}


// [[Rcpp::export]]
List cpp_m_beta(int k_R){
        // computes the parameters and the contribution to the LL
        // the values of X and tau are already loaded

        int k = k_R - 1;

        // retrieving the information
        NumericMatrix &tau = tau_prms.tau;
        NumericVector &sum_tau = tau_prms.sum_tau;

        NumericMatrix &X = X_prms.X;
        int n = X_prms.n;
        int M = tau_prms.M;

        // STEP 1: Computing the parameters

        // We need some statistics to perform the NR algorithm
        NumericVector ln_x(M);
        NumericVector ln_1_x(M);
        NumericVector s_mean(M);
        NumericVector s_var(M);

        for(int m=0 ; m<M ; ++m){
            double x_tmp;
            double tau_tmp;
            for(int i=0 ; i<n ; ++i){
                x_tmp = X(i, k);
                tau_tmp = tau(i, m);

                ln_x(m)   += tau_tmp * log(x_tmp);
                ln_1_x(m) += tau_tmp * log(1 - x_tmp);
                tau_tmp *= x_tmp;
                s_mean(m) += tau_tmp;
                s_var(m)  += tau_tmp * x_tmp;
            }
        }

        // Normalization
        for(int m=0 ; m<M ; ++m){
            ln_x(m)   /= sum_tau[m];
            ln_1_x(m) /= sum_tau[m];

            s_mean(m) /= sum_tau[m];
            s_var(m)  /= sum_tau[m];

            s_var(m)  -= s_mean(m) * s_mean(m);
        }

        // Now the NR algorithm
        NumericMatrix prms = NR_beta(ln_x, ln_1_x, s_mean, s_var);

        // STEP 2: contribution to the LL

        NumericMatrix ll(n, M);

        for(int m=0 ; m<M ; ++m){
            double x_tmp;
            double alpha = prms(0, m);
            double beta  = prms(1, m);
            double constant = lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta);

            for(int i=0 ; i<n ; ++i){
                x_tmp = X(i, k);
                ll(i, m) = (alpha + 1) * log(x_tmp) + (beta + 1) * log(1 - x_tmp) + constant;
            }
        }

        List res;
        res["prms"] = prms;
        res["ll"] = ll;

        return res;
}

// [[Rcpp::export]]
NumericMatrix cpp_m_step(StringVector method){
    // The full m-step
    
    int K = X_prms.K;
    int n = X_prms.n;
    
    int M = tau_prms.M;
    
    // The ll matrix of individual contributions
    NumericMatrix ll_obs(n, M);
    
    for(int k=0 ; k<K ; ++k){
        // we compute the parameters and the individual contributions
        // for each family and variable

        NumericMatrix ll;
        
        if(method[k] == "multinomial"){
            ll = multinom_contribution(k);
        } else if(method[k] == "normal"){
            ll = normal_contribution(k);
        } else if(method[k] == "beta"){
            ll = beta_contribution(k);
        } else {
            stop("Internal error: method not available");
        }
        
        for(int m=0 ; m<M ; ++m){
            for(int i=0 ; i<n ; ++i){
                ll_obs(i, m) += ll(i, m);
            }
        }
        
    }
    
    // We also compute delta, the proba to be of class m
    NumericVector &sum_tau = tau_prms.sum_tau;
    NumericVector delta(M);
    double total = 0;
    for(int m=0 ; m<M ; ++m) total += sum_tau[m];
    for(int m=0 ; m<M ; ++m) delta[m] = log(sum_tau[m] / total);

    for(int m=0 ; m<M ; ++m){
        double d = delta[m];
        for(int i=0 ; i<n ; ++i){
            ll_obs(i, m) += d;
        }
    }
    
    
    return ll_obs;
}


// [[Rcpp::export]]
IntegerVector cpp_em_influence(NumericMatrix ll_obs, StringVector method){
    // The influence of each variable-family on classification
    
    int K = X_prms.K;
    int n = X_prms.n;
    
    int M = tau_prms.M;
    
    IntegerVector classes(n);
    
    // we find the class of each observation
    double value;
    double m_max;
    for(int i=0 ; i<n ; ++i){
        value = ll_obs(i, 0);
        m_max = 0;
        for(int m=1 ; m<M ; ++m){
            if(value < ll_obs(i, m)){
                m_max = m;
                if(m < M){
                    value = ll_obs(i, m);
                }
            }
        }
        classes(i) = m_max;
    }
    
    
    // the vector of changes
    IntegerVector n_swaps(K);
    
    for(int k=0 ; k<K ; ++k){
        // we compute the parameters and the individual contributions
        // for each family and variable
        
        NumericMatrix ll;
        
        if(method[k] == "multinomial"){
            ll = multinom_contribution(k);
        } else if(method[k] == "normal"){
            ll = normal_contribution(k);
        } else if(method[k] == "beta"){
            ll = beta_contribution(k);
        } else {
            stop("Internal error: method not available");
        }
        
        for(int i=0 ; i<n ; ++i){
            value = ll_obs(i, 0) - ll(i, 0);
            m_max = 0;
            for(int m=1 ; m<M ; ++m){
                if(value < ll_obs(i, m) - ll(i, m)){
                    m_max = m;
                    if(m < M){
                        value = ll_obs(i, m) - ll(i, m);
                    }
                }
            }
            
            if(m_max != classes[i]) n_swaps[k]++;
        }
        
    }
    
    return n_swaps;
}

// [[Rcpp::export]]
List cpp_get_params(StringVector method, bool final = true){
    // The full m-step
    
    int K = X_prms.K;
    
    // res: list of length K, the parameters for each variable-family
    List res;
    
    for(int k=0 ; k<K ; ++k){
        // we compute the parameters for each family and variable
        
        NumericMatrix prms;
        
        // Rcout << k << ", " << method[k] << "\n";
        
        if(method[k] == "multinomial"){
            prms = multinom_params(k, final);
        } else if(method[k] == "normal"){
            prms = normal_params(k);
        } else if(method[k] == "beta"){
            prms = beta_params(k);
        } else {
            stop("Internal error: method not available");
        }
        
        // Rcout << "ok\n";
        
        res.push_back(prms);
        
    }
    
    // We also compute delta, the proba to be of class m
    NumericVector &sum_tau = tau_prms.sum_tau;
    int M = tau_prms.M;
    NumericVector delta(M);
    double total = 0;
    for(int m=0 ; m<M ; ++m) total += sum_tau[m];
    for(int m=0 ; m<M ; ++m) delta[m] = sum_tau[m] / total;
    
    res.push_back(delta);
    
    return res;
}

