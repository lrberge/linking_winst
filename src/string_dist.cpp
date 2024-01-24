/* *****************************************************************************
 *                                                                             *   
 * Author: Laurent R. Berge                                                    *
 * Purpose: low level string distance computation, in parallel                 *
 *                                                                             *
 ******************************************************************************/

#include <Rcpp.h>
#include <cstring>
#include <vector>
#ifdef _OPENMP
  #include <omp.h>
#else
  #define omp_get_max_threads() 0
#endif

using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export]]
int get_nb_threads(){
  int res = omp_get_max_threads();
  return(res);
}

// The Jaro-Winkler is an adaptation from the following code source:
// https://github.com/TriviaMarketing/Jaro-Winkler

double jaro_single(const char * str_1, const char * str_2){
  
  // Register strings length.
  int len_1 = std::strlen(str_1);
  int len_2 = std::strlen(str_2);
  
  // Calculate max length range.
  int maxRange(std::max(0, std::max(len_1, len_2) / 2 - 1));
  
  // Creates 2 vectors of integers.
  std::vector<bool> match_1(len_1, false);
  std::vector<bool> match_2(len_2, false);
  
  // Calculate matching characters.
  int n_match = 0;
  for(int i_1 = 0; i_1 < len_1; ++i_1){
    
    // Calculate window test limits (limit inferior to 0 and superior to len_2).
    int minIndex = std::max(i_1 - maxRange, 0);
    int maxIndex = std::min(i_1 + maxRange + 1, len_2);
    
    if (minIndex >= maxIndex){
      // No more common character because we don't have characters in b to test with characters in a.
      break;
    }
    
    for(int i_2 = minIndex ; i_2 < maxIndex; ++i_2){
      if(str_1[i_1] == str_2[i_2] && !match_2[i_2]){
        match_1[i_1] = true;
        match_2[i_2] = true;
        ++n_match;
        break;
      }
    }
  }
  
  // If no matching characters, we return 0.
  if (n_match == 0){
    return 0.0;
  }
  
  // Calculate character transpositions.
  std::vector<int> position_1(n_match, 0);
  std::vector<int> position_2(n_match, 0);
  
  for(int i_1 = 0, positionIndex = 0; i_1 < len_1; ++i_1){
    if(match_1[i_1]){
      position_1[positionIndex] = i_1;
      ++positionIndex;
    }
  }
  
  for(int i_2 = 0, positionIndex = 0; i_2 < len_2; ++i_2){
    if(match_2[i_2]){
      position_2[positionIndex] = i_2;
      ++positionIndex;
    }
  }
  
  // Counting half-transpositions.
  int n_transpositions = 0;
  for(int index = 0; index < n_match; ++index){
    if(str_1[position_1[index]] != str_2[position_2[index]]){
      ++n_transpositions;
    }
  }
  
  // Calculate Jaro distance
  double dmatch = n_match;
  double res = (dmatch / len_1 + dmatch / len_2 + (dmatch - n_transpositions / 2) / dmatch) / 3.0;
  
  return res;
}



double jaroWinkler_single(const char * str_1, const char * str_2, int min_char, double empty_val){
  
  int len_1 = std::strlen(str_1);
  int len_2 = std::strlen(str_2);
  
  // If one string has null length, we return 0.
  if(len_1 == 0 && len_2 == 0){
    return empty_val;
  } 
  
  if(len_1 < min_char || len_2 < min_char){
    
    if(len_1 == len_2){
      
      for(int i=0 ; i<len_1 ; ++i){
        if(str_1[i] != str_2[i]){
          return 0.0;
        }
      }
      
      return 1.0;
    }
    
    
    return 0.0;
  }
  
  // Calculate Jaro distance.
  double distance = jaro_single(str_1, str_2);
  
  if(distance == 1 || distance == 0){
    return(distance);
  }
  
  // The max length of the common prefix
  int n_max = std::min(std::min(len_1, len_2), 4);
  
  // Calculate common string prefix.
  int n_prefix = 0;
  for(int i = 0 ; i < n_max; ++i){
    if(str_1[i] == str_2[i]){
      ++n_prefix;
    } else {
      break;
    }
  }
  
  // Calculate Jaro-Winkler distance.
  distance += 0.1 * n_prefix * (1.0 - distance);
  
  return distance;
}


// [[Rcpp::export]]
NumericVector cpp_jaroWinkler(SEXP Rstr_1, SEXP Rstr_2, int min_char = 0, double empty_val = 0, int nthreads = 1){
  
  int n_1 = LENGTH(Rstr_1);
  int n_2 = LENGTH(Rstr_2);
  
  // We allow recycling
  int n_max = std::max(n_1, n_2);
  
  NumericVector res(n_max);
  
#pragma omp parallel for num_threads(nthreads)
  for(int i=0 ; i<n_max ; ++i){
    const char *str_1 = CHAR(STRING_ELT(Rstr_1, i % n_1));
    const char *str_2 = CHAR(STRING_ELT(Rstr_2, i % n_2));
    
    res[i] = jaroWinkler_single(str_1, str_2, min_char, empty_val);
  }
  
  
  return res;
}


double unitary_single(const char * str_1, const char * str_2, int min_char, double empty_val){
  
  int len_1 = std::strlen(str_1);
  int len_2 = std::strlen(str_2);
  
  // Unitary distance: if diff greater than 2 => out
  if(len_1 == 0 || len_2 == 0 || len_1 < min_char || len_2 < min_char || std::abs(len_1 - len_2) >= 2){
    
    // we check if the strings are identical: only condition to return 1
    
    if(len_1 == len_2){
      
      if(len_1 == 0){
        return empty_val;
      }
      
      for(int i=0 ; i<len_1 ; ++i){
        if(str_1[i] != str_2[i]){
          return 0.0;
        }
      }
      
      return 1.0;
    } 
    
    // strings are of different length and one < minimum length
    return 0.0;
    
  }
  
  const char * s_short;
  const char * s_long;
  
  if(len_1 > len_2){
    s_short = str_2;
    s_long = str_1;
  } else {
    s_short = str_1;
    s_long = str_2;
  }
  
  int i = 0;
  while(i < len_1){
    if(s_short[i] == s_long[i]){
      ++i;
    } else {
      if(len_1 == len_2){
        // => que inversion et replacement a tester
        
        // inversion
        if(i < len_1 - 1 && s_short[i + 1] != s_long[i + 1]){
          if(s_short[i] == s_long[i + 1] && s_short[i + 1] == s_long[i]){
            i += 2;
          } else {
            return 0;
          }
        } else {
          ++i;
        }
        
        // We test that the rest of the string is OK
        for(; i < len_1 ; ++i){
          if(s_short[i] != s_long[i]){
            return 0;
          }
        }
        
      } else {
        // => que addition a tester
        if(s_short[i] != s_long[i + 1]){
          return 0;
        } else {
          // we check the rest of the string is fine
          ++i;
          for(; i < len_1 ; ++i){
            if(s_short[i] != s_long[i + 1]){
              return 0;
            }
          }
        }
      }
    }
  }
  
  // If we're here => fine
  return 1;
}




// [[Rcpp::export]]
NumericVector cpp_unitary_stringDist(SEXP Rstr_1, SEXP Rstr_2, int min_char = 0, double empty_val = 0, int nthreads = 1){
  
  int n_1 = LENGTH(Rstr_1);
  int n_2 = LENGTH(Rstr_2);
  
  // We allow recycling
  int n_max = std::max(n_1, n_2);
  
  NumericVector res(n_max);
  
#pragma omp parallel for num_threads(nthreads)
  for(int i=0 ; i<n_max ; ++i){
    const char *str_1 = CHAR(STRING_ELT(Rstr_1, i % n_1));
    const char *str_2 = CHAR(STRING_ELT(Rstr_2, i % n_2));
    
    res[i] = unitary_single(str_1, str_2, min_char, empty_val);
  }
  
  
  return res;
}


inline bool is_end_word(const char &ch){
  return !((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'));
}

inline bool is_letter(const char &ch){
  return !((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'));
}


inline bool is_included_words(const char * s_short, const char * s_long, int i, int j){
  // checks that the short string is included in the long string 
  // with specific starting points for the short and long strings
  // we ensure that at the end of the short string we are at the end of the long string or at a space 
  
  int len_short = std::strlen(s_short);
  int len_long = std::strlen(s_long);
  
  int k = 0;
  while(i + k < len_short && j + k < len_long && s_short[i + k] == s_long[j + k]) ++k;
  
  return i + k == len_short && (j + k == len_long || is_end_word(s_long[j + k]));
}


double inclusion_strict(const char * str_1, const char * str_2, bool misspell, int min_char, double empty_val){
  // even if misspell is tolerated, we require the first letter to match
  // This is a word inclusion algorithm:
  // we check whether "w2 w3" is included in "w1 w2 w3" (for example)
  // this is not a pattern inclusion!
  // eg "bon" is NOT included in "bonjour les gens" because "bon" is not a word in the second string
  //     alhtough the pattern "bon" is included in the second string
  
  int len_1 = std::strlen(str_1);
  int len_2 = std::strlen(str_2);
  
  const char * s_short;
  const char * s_long;
  int len_short = len_1, len_long = len_2;
  
  if(len_1 > len_2){
    s_short = str_2;
    s_long = str_1;
    len_short = len_2;
    len_long = len_1;
  } else {
    s_short = str_1;
    s_long = str_2;
  }
  
  if(len_short == 0 || len_short < min_char){
    
    // we check if the strings are identical
    if(len_short == len_long){
      
      if(len_short == 0){
        return empty_val;
      }
      
      for(int i=0 ; i<len_short ; ++i){
        if(s_short[i] != s_long[i]){
          return 0.0;
        }
      }
      
      return 1.0;
    }
    
    
    return 0.0;
  }
  
  // int cpt = 0;
  
  int i = 0, j = 0;
  bool find_next = false;
  while(true){
    
    // we find the first letter match
    while(s_short[0] != s_long[j] || find_next){
      
      find_next = false;
      
      // we go to the next space
      while(j < len_long && s_long[j] != ' ') ++j;
      ++j; // to escape the space
      
      if(j >= len_long || len_long - j < len_short - misspell){
        // we substract 1 if misspell
        // ex if no misspell: 2 < 3 => must fail, so we end up here       (long vs short: "an" vs "and")
        //       if misspell: 2 < 3 => OK because tolerance for misspell  (long vs short: "an" vs "and" becomes OK)
        return 0.0;
      }
      
    }
    
    // simple pattern check
    i = 0;
    while(i < len_short && i + j < len_long && s_short[i] == s_long[i + j]) ++i;
    // we check we're at the end of the word for s_long
    if(i == len_short && (i + j == len_long || is_end_word(s_long[i + j]))) return 1.0;
    
    if(misspell){
      
      if(i == len_short){
        // means the short string is fully included in the long string
        // now we check that, with 1 error allowed, we're at the end of a word in the long string
        if(i + j + 1 == len_long || is_end_word(s_long[i + j + 1])){
          return 1.0;
        } else {
          // We have 2+ letters to fill => not unitary
          return 0.0;
        }
      } 
      
      if(i == len_short - 1){
        if(i + j == len_long || is_end_word(s_long[i + j]) || i + j + 1 == len_long || is_end_word(s_long[i + j + 1])){
          // Example. Short: "bon"
          // i + j == len_long             : "bon" -- "bo"
          // is_end_word(s_long[i + j])    : "bon" -- "bo bun"
          // i + j + 1 == len_long         : "bon" -- "bou"
          // is_end_word(s_long[i + j + 1]): "bon" -- "bou than"
          return 1.0;
        } else {
          // "bon" -- "boXX"
          return 0.0;
        }
      }
      // after this line:
      // i     < len_short - 1
      // i + j < len_long
      
      bool lng_gt_2 = i + j + 1 < len_long;
      
      // TYPO
      // example:
      //   short: "bonjour", long: "ils dirent bonjoor" (bonjo_r)
      if(is_included_words(s_short, s_long, i + 1, i + j + 1)){
        return 1.0;
      }
      
      // inversion
      // example:
      //   short: "bonjour", long: "ils dirent bonojur" (bon__ur)
      if(s_short[i + 1] == s_long[i + j] && lng_gt_2 && s_short[i] == s_long[i + j + 1]){
        
        if(is_included_words(s_short, s_long, i + 2, i + j + 2)){
          return 1.0;
        }
      }
      
      // addition in long string
      // example:
      //   short: "bonjour", long: "ils dirent boinjour" (bo_njour)
      if(lng_gt_2 && s_short[i] == s_long[i + j + 1]){
        
        if(is_included_words(s_short, s_long, i + 1, i + j + 2)){
          return 1.0;
        }
      }
      
      // deletion in long string
      // example:
      //   short: "bonjour", long: "ils dirent bonjur"
      if(s_short[i + 1] == s_long[i + j]){
        
        if(is_included_words(s_short, s_long, i + 2, i + j + 1)){
          return 1.0;
        }
      }
      
    }
    
    find_next = true;
    
  }
  
  
}

inline bool is_included_string(const char * s_short, int len_short, const char * s_long, int i, int j){
  // we check if the short string is included in the long one
  // no need for the long string to end at a word delimiter
  
  int len_long = std::strlen(s_long);
  
  int k = 0;
  while(i + k < len_short && j + k < len_long && s_short[i + k] == s_long[j + k]) ++k;
  
  return i + k == len_short;
}


inline bool is_included_misspell(const char * s_short, int len_short, const char * s_long, int i, int j, int k){
  // is short included in long? 
  // we look at short up to len_short
  
  int len_long = std::strlen(s_long);
  
  if(i + k == len_short - 1){
    // this is ok (I don't really like this loose matching with misspell, there must be more sensible decision rules, 
    //             but it's really complex to make them general)
    // S:"julien est un pies" L:"julien est un pieton"
    return true;
  }
  
  bool lng_gt_2 = j + k + 1 < len_long;
  
  // FAUTE
  // example:
  //   short: "bonjour", long: "ils dirent bonjoor" (bonjo_r)
  if(is_included_string(s_short, len_short, s_long, i + k + 1, j + k + 1)){
    return true;
  }
  
  // inversion
  // example:
  //   short: "bonjour", long: "ils dirent bonojur" (bon__ur)
  if(s_short[i + k + 1] == s_long[j + k] && lng_gt_2 && s_short[i + k] == s_long[j + k + 1]){
    if(is_included_string(s_short, len_short, s_long, i + k + 2, j + k + 2)){
      return true;
    }
  }
  
  // addition in long string
  // example:
  //   short: "bonjour", long: "ils dirent boinjour" (bo_njour)
  if(lng_gt_2 && s_short[i + k] == s_long[j + k + 1]){
    if(is_included_string(s_short, len_short, s_long, i + k + 1, j + k + 2)){
      return true;
    }
  }
  
  // deletion in long string
  // example:
  //   short: "bonjour", long: "ils dirent bonjur"
  // Rcout << " index i: " << i + k + 1 << ", " << s_short[i + k + 1];
  // Rcout ", index j: " << j + k + 1 << ", " << s_long[j + k] << "\n";
  if(s_short[i + k + 1] == s_long[j + k]){
    if(is_included_string(s_short, len_short, s_long, i + k + 2, j + k + 1)){
      return true;
    }
  }
  
  return false;
}


double inclusion_loose(const char * str_1, const char * str_2, bool misspell, int min_char, double empty_val){
  // The difference with the inclusion strict is twofold:
  // - "bon" would be included in "hello, bonjour"
  // - this is on a word by word basis: "bon gens" would be included in "hello, bonjour les gens"
  // We always require the first letters to match
  
  int len_1 = std::strlen(str_1);
  int len_2 = std::strlen(str_2);
  
  
  if(len_1 == 0 || len_1 < min_char || len_2 == 0 || len_2 < min_char){
    
    // we check if the strings are identical
    if(len_1 == len_2){
      
      if(len_1 == 0){
        return empty_val;
      }
      
      for(int i=0 ; i<len_1 ; ++i){
        if(str_1[i] != str_2[i]){
          return 0.0;
        }
      }
      
      return 1.0;
    }
    
    return 0.0;
  }
  
  const char * s_short;
  const char * s_long;
  int len_short = len_1, len_long = len_2;
  
  bool is_two_short = len_2 < len_1;
  
  if(len_1 == len_2){
    // in case of equality, we place the one with ' ' as the short one
    // the order matters here because we will test the word inclusion only for the short one
    
    int n_space_1 = 0, n_space_2 = 0;
    
    for(int i=0 ; i<len_1 ; ++i){
      if(str_1[i] == ' ') ++n_space_1;
    }
    
    // we count only if there are spaces in str 1
    // otherwise the default is to set the second as the short w/t checking
    if(n_space_1 == 0){
      n_space_2 = 10;
      
    } else if(n_space_1 > 0){
      for(int i=0 ; i<len_2 ; ++i){
        if(str_2[i] == ' ') ++n_space_2;
      }
    }
    
    is_two_short = n_space_2 > n_space_1;
  } 
  
  if(is_two_short){
    s_short = str_2;
    s_long = str_1;
    len_short = len_2;
    len_long = len_1;
  } else {
    s_short = str_1;
    s_long = str_2;
  }
  
  // Rcout << "short: '" << s_short << "'\n";
  // Rcout << "long: '" << s_long << "'\n";
  
  // i: starting index of the short string
  // j: starting index of the long string
  // k: moving index
  
  bool is_one_word = false;
  bool len_word = 0;
  
  int i = 0, j = 0, k = 0;
  bool find_next_j = false;
  while(true){
    
    // we find the first letter match
    while(s_short[i] != s_long[j] || find_next_j){
      
      find_next_j = false;
      
      // we go to the next space
      while(j < len_long && s_long[j] != ' ') ++j;
      ++j; // to escape the space
      
      if(j >= len_long || len_long - j < (len_short - i) - misspell){
        // we substract 1 if misspell
        // ex if no misspell: 2 < 3 => must fail, so we end up here       (long vs short: "an" vs "and")
        //       if misspell: 2 < 3 => OK because tolerance for misspell  (long vs short: "an" vs "and" becomes OK)
        // either there is only one word left in "s_short" => we can tolerate 1 longer because of the misspell
        // either there are 2+ words and the two words should fit into the smaller string => not working either
        return 0.0;
      }
      
    }
    
    //  I) Full string matching
    
    // simple pattern check
    k = 0;
    while(i + k < len_short && j + k < len_long && s_short[i + k] == s_long[j + k]) ++k;
    
    // A) it went all the way in s_short
    //    => this means that the pattern of s_short is included in s_long: match (no need to check for end of word in s_long)
    if(i + k == len_short) return 1.0;
    
    // Rcout << "k = " << k;
    
    // B) we didn't go to the end, but would we with a misspell?
    if(misspell && is_included_misspell(s_short, len_short, s_long, i, j, k)){
      return 1.0;
    }
    
    if(is_one_word){
      // the remaining string is just one word, so we can carry on (trying other values in long)
      find_next_j = true;
      continue;
    }
    
    
    // C) OK none has matched.
    // we go back and see if at least one word of s_short has been matched
    //      i) one word has been matched. We go to the next word (we change i)
    //         ex: S:"john snow" L:"jonh doesn't like snow". Here john is matched and we go to the next item
    //     ii) there is no match, we go to the next word in s_long
    
    // We fetch the beginning of the current word
    int k_bak = k;
    while(k > 0 && s_short[i + k] != ' ') --k;
    
    // we go back to the first word
    if(s_short[i + k] == ' '){
      // hello Julien Genty    
      //        |               
      // hello Jenty           
      //        |               
      // Now we restart for Genty from Julien
      ++k;
      i += k;
      j += k;
      
      // we restart from Jenty in short, we don't move in long (Julien)
      find_next_j = false;
      continue;                                                 // SHORT-CIRCUIT
    } else {
      // we didnt change words
      // jambon Julien Genty    
      //  |               
      // julien Genty           
      //  |               
      
      // Is s_short only one word?
      k = k_bak;
      while(i + k < len_short && s_short[i + k] != ' ') ++k;
      
      if(i + k == len_short){
        // OK, s_short is just one word, we carry on
        is_one_word = true;
        find_next_j = true;
        continue;                                               // SHORT-CIRCUIT
      }
      
      // now we try to match the current word
      len_word = i + k;
    }
    
    // II) word matching
    
    k = 0;
    while(i + k < len_word && j + k < len_long && s_short[i + k] == s_long[j + k]) ++k;
    
    // A) it went all the way in s_short
    bool matched = (i + k == len_short);
    
    // B) we didn't go to the end, but would we with a misspell?
    if(misspell && is_included_misspell(s_short, len_short, s_long, i, j, k)){
      matched = true;
    }
    
    if(matched){
      // the word is matched! (only a word, a full string would return 1)
      // => we go to the next word and the next loop
      
      // we go to the next word in s_short
      i = len_word;
      while(i < len_short && s_short[i] != ' ') ++i;
      ++i;
      
      // special case of trailing space
      if(i >= len_short) return 1.0;
      
      // we go to the next word in s_long
      j += (len_word - i);
      while(j < len_long && s_long[j] != ' ') ++j;
      ++j;
      
      // special case of trailing space
      if(j >= len_long) return 1.0;
      
      find_next_j = false;
      misspell = false;
      
    } else {
      // we go to the next word in s_long
      find_next_j = true;
    }
  }
}


// [[Rcpp::export]]
NumericVector cpp_inclusion(SEXP Rstr_1, SEXP Rstr_2, bool loose, bool misspell, 
                            int min_char = 0, double empty_val = 0, int nthreads = 1){
  
  int n_1 = LENGTH(Rstr_1);
  int n_2 = LENGTH(Rstr_2);
  
  // We allow recycling
  int n_max = std::max(n_1, n_2);
  
  NumericVector res(n_max);
  
#pragma omp parallel for num_threads(nthreads)
  for(int i=0 ; i<n_max ; ++i){
    const char *str_1 = CHAR(STRING_ELT(Rstr_1, i % n_1));
    const char *str_2 = CHAR(STRING_ELT(Rstr_2, i % n_2));
    
    if(loose){
      res[i] = inclusion_loose(str_1, str_2, misspell, min_char, empty_val);
    } else {
      res[i] = inclusion_strict(str_1, str_2, misspell, min_char, empty_val);
    }
    
  }
  
  
  return res;
}









































