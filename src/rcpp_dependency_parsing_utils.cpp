#include <Rcpp.h>

/*
 * Functionalities to speed up working with dependency parsing results
 */
std::vector<int> pluck_int(const Rcpp::List& x, const unsigned int& i) {
  if(i >= x.size()){
    Rcpp::Rcout << "Trying to extract list element " << i << "/" << x.size() << std::endl;  
    Rcpp::stop("This is not possible");
  }
  std::vector<int> rows = x[i-1];
  return rows;
}

std::tuple<std::vector<int>, std::vector<int>, std::vector<unsigned int> > dependency_rowlocations_recursive(const unsigned int& row, const Rcpp::List& x, const int depth = 1) {
  std::vector<int> newrows = pluck_int(x, row);
  int n = newrows.size();
  std::vector<int> newdepth;
  std::vector<unsigned int> from;
  for(int j = 0; j < n; j++){
    newdepth.push_back(depth);
    from.push_back(row);
  }
  for(int j = 0; j < n; j++){
    std::tuple<std::vector<int>, std::vector<int>, std::vector<unsigned int> > extra = dependency_rowlocations_recursive(newrows[j], x, depth + 1);
    int n_extra = std::get<0>(extra).size();
    for(int k = 0; k < n_extra; k++){
      newrows.push_back(std::get<0>(extra)[k]);
      newdepth.push_back(std::get<1>(extra)[k]);
      from.push_back(std::get<2>(extra)[k]);
    }
  }
  return std::make_tuple(newrows, newdepth, from);
}

// [[Rcpp::export]]
Rcpp::List dependency_rowlocations(const unsigned int& row, const Rcpp::List& x, const int depth = 1){
  std::tuple<std::vector<int>, std::vector<int>, std::vector<unsigned int> > info = dependency_rowlocations_recursive(row, x, depth);
  Rcpp::List output = Rcpp::List::create(Rcpp::Named("row")   = std::get<0>(info), 
                                         Rcpp::Named("depth") = std::get<1>(info),
                                         Rcpp::Named("from")  = std::get<2>(info));
  return output;
}

