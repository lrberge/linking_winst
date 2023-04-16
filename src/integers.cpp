/* *****************************************************************************
 *                                                                             *   
 * Author: Laurent R. Berge                                                    *
 * Purpose: turning vectors into integer indexes                               *
 *                                                                             *
 ******************************************************************************/

#include <stdint.h>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// <stdint.h>: uint32_t

// unions: only one space in memory. Interprets the data in memory in different ways.
union d2int {
  double dbl;
  uint32_t uint[2]; 
};

inline uint32_t hash_to_int(uint32_t value, int shifter){
  return (3141592653U * value >> (32 - (shifter)));
}

inline uint32_t get_value_sexp_i(int type_x, int *px_int, double *px_dbl, intptr_t *px_intptr, 
                                 int i, union d2int u_d2int){
  // we extract the value from a RSEXP
  
  uint32_t value = 0;
  if(type_x == REALSXP){
    u_d2int.dbl = px_dbl[i];
    value = u_d2int.uint[0] + u_d2int.uint[1];
  } else if(type_x == INTSXP){
    value = px_int[i];
  } else if(type_x == STRSXP){
    value = px_intptr[i] & 0xffffffff;
  } else {
    Rf_error("wrong type: internal error");
  }
  
  return value;
}

inline bool is_same_xi(int type_x, int *px_int, double *px_dbl, intptr_t *px_intptr, 
                       int i, int j){
  // are two values of x identical? 
  // this function is type agnostic
  
  bool res = false;
  if(type_x == REALSXP){
    res = px_dbl[i] == px_dbl[j];
  } else if(type_x == INTSXP){
    res = px_int[i] == px_int[j];
  } else if(type_x == STRSXP){
    res = px_intptr[i] == px_intptr[j];
  } else {
    Rf_error("wrong type: internal error");
  }
  
  return res;
}

// [[Rcpp::export]]
IntegerVector cpp_to_integer(SEXP x){
  // only accepts atomic vectors, of type: int/real/char
  
  int n = Rf_length(x);
  int type_x = TYPEOF(x);
  
  // we find out the number of bits (see shifter)
  // we find the first multiple of 2 greater than n
  const size_t n2 = 2U * (size_t) n;
  size_t n2_powOf2 = 256;
  int shifter = 8;
  
  while (n2_powOf2 < n2) {
    n2_powOf2 *= 2;
    ++shifter;
  }
  
  std::vector<int> h(n2_powOf2, -1);
  IntegerVector res(n, 0);
  uint32_t id = 0;
  union d2int u_d2int;
  int g = 0;
  
  void * px;
  if(type_x == INTSXP){
    px = INTEGER(x);
  } else if(type_x == REALSXP){
    px = REAL(x);
  } else if(type_x == STRSXP){
    px = STRING_PTR(x);
  } else {
    Rf_error("Internal error: wrong type in x.");
  }
  
  int *px_int = (int *) px;
  double *px_dbl = (double *) px;
  intptr_t *px_intptr = (intptr_t *) px;
  
  uint32_t value = 0;
  for(int i=0 ; i<n ; ++i){
    value = get_value_sexp_i(type_x, px_int, px_dbl, px_intptr, i, u_d2int);
    id = hash_to_int(value, shifter);
    
    bool does_exist = false;
    // here this is done to avoid collision
    // when there is a collision, we move forward
    // -1 is when h is not yet set 
    while(h[id] != - 1){
      // does the value exist already?
      if(is_same_xi(type_x, px_int, px_dbl, px_intptr, h[id], i)){
        res[i] = res[h[id]];
        does_exist = true;
        break;
        
      } else {
        // we increment the id (the hash already exists for a different value!)
        ++id;
        if(id > n2_powOf2){
          id %= n2_powOf2;
        } 
      }
    }
    
    if(!does_exist){
      // hash never seen => ok
      h[id] = i;
      res[i] = ++g;
    }
  }
  
  return res;
}


// [[Rcpp::export]]
int find_shifter(int64_t x){
  int r = 0;
  int64_t z(x);
  
  while(z){
    z = z >> 1;
    ++r;
  }
  
  return r;
}


// [[Rcpp::export]]
NumericVector cpp_multi_integer(SEXP Rlist){
  // list of integer vectors, all of the same length
  // returns a single numeric vector
  
  int K = Rf_length(Rlist);
  
  SEXP x = VECTOR_ELT(Rlist, 0);
  int *px = INTEGER(x);
  
  int n = Rf_length(x);
  
  std::vector<int64_t> res(n);
  
  // we copy and find the max
  int64_t n_cases = 1; // equals to the max
  for(int i=0 ; i<n ; ++i){
    if(px[i] > n_cases){
      n_cases = px[i];
    }
    res[i] = px[i];
  }
  
  int shifter = find_shifter(n_cases);
  
  if(K == 1){
    NumericVector res_num(n);
    for(int i=0 ; i<n ; ++i){
      res_num[i] = static_cast<double>(res[i]);
    }
    return res_num;
  }
  
  for(int k=1 ; k<K ; ++k){
    
    x = VECTOR_ELT(Rlist, k);
    px = INTEGER(x);
    
    // we shift and add
    int64_t val = 0;
    for(int i=0 ; i<n ; ++i){
      val = static_cast<int64_t>(px[i]) << shifter;
      res[i] += val;
    }
    
    if(k != K){
      int64_t nc = px[0];
      for(int i=0 ; i<n ; ++i){
        if(px[i] > nc){
          nc = px[i];
        }
      }
      shifter += find_shifter(nc);
      n_cases += nc;
    }
  }
  
  NumericVector res_num(n);
  for(int i=0 ; i<n ; ++i){
    res_num[i] = static_cast<double>(res[i]);
  }
  
  return res_num;
}






