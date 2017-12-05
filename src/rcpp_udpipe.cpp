#include <Rcpp.h>
#include <fstream>
#include <memory>
#include "udpipe.h"


/*
 * Functionalities to tokenise, tag and do dependency parsing based on a model
 */

// [[Rcpp::export]]
SEXP udp_load_model(const char* file_model) {
  // Load language model and return the pointer to be used by udp_tokenise_tag_parse
  ufal::udpipe::model *languagemodel;
  languagemodel = ufal::udpipe::model::load(file_model);
  Rcpp::XPtr<ufal::udpipe::model> ptr(languagemodel, true);
  return ptr;
}

// [[Rcpp::export]]
Rcpp::List udp_tokenise_tag_parse(SEXP udmodel, Rcpp::StringVector x, Rcpp::StringVector docid, 
                            std::string annotation_tokenizer,
                            std::string annotation_tagger,
                            std::string annotation_parser) {
  Rcpp::XPtr<ufal::udpipe::model> languagemodel(udmodel);
  
  // Handle default and none input to tokenizer, tagger, parser
  std::string pipeline_tokenizer = annotation_tokenizer;
  std::string pipeline_tagger = annotation_tagger;
  std::string pipeline_parser = annotation_parser;
  if (pipeline_tagger.compare("none") == 0){
    pipeline_tagger = ufal::udpipe::pipeline::NONE;
  }else if (pipeline_tagger.compare("default") == 0){
    pipeline_tagger = ufal::udpipe::pipeline::DEFAULT;
  }
  if (pipeline_parser.compare("none") == 0){
    pipeline_parser = ufal::udpipe::pipeline::NONE;
  }else if (pipeline_parser.compare("default") == 0){
    pipeline_parser = ufal::udpipe::pipeline::DEFAULT;
  }
  
  // Set up pipeline: tokenizer, tagger and dependency parser, output format conllu
  ufal::udpipe::pipeline languagemodel_pipeline = ufal::udpipe::pipeline(languagemodel, pipeline_tokenizer, pipeline_tagger, pipeline_parser, "conllu");
  
  // Put character data in a stream
  std::string error;
  std::string doc_id;
  std::istringstream iss;
  std::ostringstream oss;
  std::vector< std::string > result(x.size()); 
  
  int i;
  for (i = 0; i < x.size(); i++){
    iss.str (Rcpp::as<std::string>(x[i]));
    doc_id = Rcpp::as<std::string>(docid[i]);
    languagemodel_pipeline.set_document_id(doc_id);
    languagemodel_pipeline.process(iss, oss, error);
    iss.clear();
    result[i] = error;
  }
  
  // Get the output stream back in std::string
  std::string data =  oss.str();
  
  // Return
  Rcpp::List output = Rcpp::List::create(Rcpp::Named("x") = x, 
                             Rcpp::Named("conllu") = data,
                             Rcpp::Named("errors") = result);
  return output;
}



// [[Rcpp::export]]
Rcpp::List udp_evaluate(SEXP udmodel, 
                        Rcpp::CharacterVector conllu_test_file, 
                        Rcpp::CharacterVector output_file, 
                        std::string annotation_tokenizer,
                        std::string annotation_tagger,
                        std::string annotation_parser) {
  Rcpp::XPtr<ufal::udpipe::model> languagemodel(udmodel);
  
  // Handle default and none input to tokenizer, tagger, parser
  std::string pipeline_tokenizer = annotation_tokenizer;
  std::string pipeline_tagger = annotation_tagger;
  std::string pipeline_parser = annotation_parser;
  if (pipeline_tagger.compare("none") == 0){
    pipeline_tagger = ufal::udpipe::pipeline::NONE;
  }else if (pipeline_tagger.compare("default") == 0){
    pipeline_tagger = ufal::udpipe::pipeline::DEFAULT;
  }
  if (pipeline_parser.compare("none") == 0){
    pipeline_parser = ufal::udpipe::pipeline::NONE;
  }else if (pipeline_parser.compare("default") == 0){
    pipeline_parser = ufal::udpipe::pipeline::DEFAULT;
  }
  
  // Set up evaluator
  ufal::udpipe::evaluator modelevaluator = ufal::udpipe::evaluator(languagemodel, pipeline_tokenizer, pipeline_tagger, pipeline_parser);
  
  // Input CONLLU filestream and output file containing the evaluation
  std::string path;
  path = conllu_test_file[0];
  std::ifstream infile(path.c_str());
  path = output_file[0];
  std::ofstream outfile(path.c_str());

  // Evaluate the model
  std::string error;
  modelevaluator.evaluate(infile, outfile, error);
  
  // Return the file and the error
  Rcpp::List output = Rcpp::List::create(Rcpp::Named("error") = error);
  return output;
}


// [[Rcpp::export]]
Rcpp::CharacterVector na_locf(Rcpp::CharacterVector x) {
  int i;
  for(i = 1; i < x.size(); i++) {
    if((x[i] == NA_STRING) & (x[i-1] != NA_STRING)) {
      x[i] = x[i-1];
    }
  }
  return x;
}


/*
 * Functionalities to train a model based on a conll-u file
 */

bool append_conllu(std::istream& is, std::vector<ufal::udpipe::sentence>& sentences, std::string& error) {
  std::unique_ptr<ufal::udpipe::input_format> conllu_input(ufal::udpipe::input_format::new_conllu_input_format());
  
  std::string block;
  while (conllu_input->read_block(is, block)) {
    conllu_input->set_text(block);
    while (sentences.emplace_back(), conllu_input->next_sentence(sentences.back(), error)) ;
    sentences.pop_back();
    if (!error.empty()) return false;
  }
  return true;
}

// [[Rcpp::export]]
Rcpp::List udp_train(const char* file_model, 
                      Rcpp::CharacterVector conllu_input_files, Rcpp::CharacterVector conllu_heldout_files,
                      std::string annotation_tokenizer,
                      std::string annotation_tagger,
                      std::string annotation_parser) {
  
  // Handle default and none input to tokenizer, tagger, parser
  std::string trainer_tokenizer = annotation_tokenizer;
  std::string trainer_tagger = annotation_tagger;
  std::string trainer_parser = annotation_parser;
  if (annotation_tokenizer.compare("none") == 0){
    trainer_tokenizer = ufal::udpipe::trainer::NONE;
  }else if (annotation_tokenizer.compare("default") == 0){
    trainer_tokenizer = ufal::udpipe::trainer::DEFAULT;
  }
  if (annotation_tagger.compare("none") == 0){
    trainer_tagger = ufal::udpipe::trainer::NONE;
  }else if (annotation_tagger.compare("default") == 0){
    trainer_tagger = ufal::udpipe::trainer::DEFAULT;
  }
  if (annotation_parser.compare("none") == 0){
    trainer_parser = ufal::udpipe::trainer::NONE;
  }else if (annotation_parser.compare("default") == 0){
    trainer_parser = ufal::udpipe::trainer::DEFAULT;
  }
  
  std::string error;
  std::vector<ufal::udpipe::sentence> training;
  std::vector<ufal::udpipe::sentence> heldout;
  std::string path;

  // Load training data
  for (int i = 0; i < conllu_input_files.size(); i++){
    path = conllu_input_files[i];
    std::ifstream input(path.c_str());
    append_conllu(input, training, error);
  }
  // Load heldout data
  for (int i = 0; i < conllu_heldout_files.size(); i++){
    path = conllu_heldout_files[i];
    std::ifstream input(path.c_str());
    append_conllu(input, heldout, error);
  }
  // Open output binary file
  std::ofstream model(file_model, std::ofstream::binary);
  // Train the model
  ufal::udpipe::trainer::train("morphodita_parsito", 
                 training, heldout, 
                 trainer_tokenizer, trainer_tagger, trainer_parser, 
                 model, error);
  
  // Return
  Rcpp::List output = Rcpp::List::create(Rcpp::Named("file_model") = file_model, 
                             Rcpp::Named("errors") = error);
  return output;
}
