#include <Rcpp.h>
#include "udpipe.h"
#include <fstream>
using namespace Rcpp;
using namespace ufal::udpipe;


/*
 * Functionalities to tokenise, tag and do dependency parsing based on a model
 */

// [[Rcpp::export]]
SEXP udp_load_model(const char* file_model) {
  // Load language model and return the pointer to be used by udp_tokenise_tag_parse
  model *languagemodel;
  languagemodel = model::load(file_model);
  Rcpp::XPtr<model> ptr(languagemodel, true);
  return ptr;
}

// [[Rcpp::export]]
List udp_tokenise_tag_parse(SEXP udmodel, Rcpp::StringVector x, Rcpp::StringVector docid, 
                            std::string annotation_tokenizer,
                            std::string annotation_tagger,
                            std::string annotation_parser) {
  Rcpp::XPtr<model> languagemodel(udmodel);
  
  // Handle default and none input to tokenizer, tagger, parser
  std::string pipeline_tokenizer = annotation_tokenizer;
  std::string pipeline_tagger = annotation_tagger;
  std::string pipeline_parser = annotation_parser;
  if (pipeline_tagger.compare("none") == 0){
    pipeline_tagger = pipeline::NONE;
  }else if (pipeline_tagger.compare("default") == 0){
    pipeline_tagger = pipeline::DEFAULT;
  }
  if (pipeline_parser.compare("none") == 0){
    pipeline_parser = pipeline::NONE;
  }else if (pipeline_parser.compare("default") == 0){
    pipeline_parser = pipeline::DEFAULT;
  }
  
  // Set up pipeline: tokenizer, tagger and dependency parser, output format conllu
  pipeline languagemodel_pipeline = pipeline(languagemodel, pipeline_tokenizer, pipeline_tagger, pipeline_parser, "conllu");
  
  // Put character data in a stream
  std::string error;
  std::string doc_id;
  std::istringstream iss;
  std::ostringstream oss;
  std::vector< std::string > result(x.size()); 
  
  int i;
  for (i = 0; i < x.size(); i++){
    iss.str (as<std::string>(x[i]));
    doc_id = as<std::string>(docid[i]);
    languagemodel_pipeline.set_document_id(doc_id);
    languagemodel_pipeline.process(iss, oss, error);
    iss.clear();
    result[i] = error;
  }
  
  // Get the output stream back in std::string
  std::string data =  oss.str();
  
  // Return
  List output = List::create(Rcpp::Named("x") = x, 
                             Rcpp::Named("conllu") = data,
                             Rcpp::Named("errors") = result);
  return output;
}



// [[Rcpp::export]]
Rcpp::CharacterVector na_locf(Rcpp::CharacterVector x) {
  Rcpp::LogicalVector ismissing = is_na(x);
  int i;
  for(i = 1; i < x.size(); i++) {
    if((x[i] == NA_STRING) & (x[i-1] != NA_STRING)) {
      x[i] = x[i-1];
    }
  }
  return x;
}

