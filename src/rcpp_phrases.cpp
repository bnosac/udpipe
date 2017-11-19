#include <Rcpp.h>
#include <regex>

// [[Rcpp::export]]
Rcpp::List phrases_regex_locate(Rcpp::StringVector x, std::string pattern, int ngram_max) {
  // Find all regular locations of regular expressions in x
  std::vector<int> found_from;
  std::vector<int> found_to;
  std::vector<std::string> x_sequence;
  int i, j, jend, nexti, n;
  std::string txtconcatenation;
  n = x.size();

  for(i = 0; i < n; i++){
    txtconcatenation = "";
    jend = ngram_max-1;
    if((i + jend) >= n){
      jend = n - i;
    }
    for(j = 0; j <= jend; j++){
      nexti = i + j;
      if(nexti < n){
        txtconcatenation = txtconcatenation + x(nexti);
        if (std::regex_match(txtconcatenation, std::regex(pattern))){
          found_from.push_back (i+1);
          found_to.push_back (nexti+1);
          x_sequence.push_back (txtconcatenation);
        }  
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("from") = found_from,
                            Rcpp::Named("to") = found_to,
                            Rcpp::Named("pattern") = x_sequence);
}