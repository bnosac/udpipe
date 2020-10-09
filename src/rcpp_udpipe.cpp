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
                            std::string annotation_parser,
                            int log_every, Rcpp::Function current_time,
                            std::string output_format = "conllu") {
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
  ufal::udpipe::pipeline languagemodel_pipeline = ufal::udpipe::pipeline(languagemodel, pipeline_tokenizer, pipeline_tagger, pipeline_parser, output_format);
  
  // Put character data in a stream
  std::string error;
  std::string doc_id;
  std::istringstream iss;
  std::ostringstream oss;
  std::vector< std::string > result(x.size()); 
  
  int i;
  for (i = 0; i < x.size(); i++){
    if(log_every > 0){
      if (i % log_every == 0){
        Rcpp::Rcout << Rcpp::as<std::string>(current_time()) << " Annotating text fragment " << i + 1 << "/" << x.size() << std::endl;  
      }  
    }
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
Rcpp::List udp_tokenise_tag_parse_basic(SEXP udmodel, Rcpp::StringVector x, Rcpp::StringVector docid, 
                                  std::string annotation_tokenizer,
                                  std::string annotation_tagger,
                                  std::string annotation_parser,
                                  int log_every, Rcpp::Function current_time) {
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
  std::string text;

  std::unique_ptr<ufal::udpipe::input_format> reader;
  if (languagemodel_pipeline.input == "tokenizer") {
    reader.reset(languagemodel_pipeline.m->new_tokenizer(languagemodel_pipeline.tokenizer));
  } else {
    reader.reset(ufal::udpipe::input_format::new_input_format(languagemodel_pipeline.input));
  }
  
  Rcpp::StringVector    out_doc_id;
  Rcpp::IntegerVector   out_sentence_id;
  Rcpp::IntegerVector   out_term_id;
  //Rcpp::IntegerVector out_start;
  //Rcpp::IntegerVector out_end;
  Rcpp::StringVector    out_token_id;
  Rcpp::StringVector    out_form;
  Rcpp::StringVector    out_lemma;
  Rcpp::StringVector    out_upos;
  Rcpp::StringVector    out_xpos;
  Rcpp::StringVector    out_feats;
  Rcpp::IntegerVector   out_head;
  Rcpp::StringVector    out_dep_rel;
  //Rcpp::StringVector  out_deps;
  Rcpp::StringVector    out_misc;
  // Rcpp::Function setEncoding("Encoding<-");
  // setEncoding(out_doc_id, "UTF-8");
  // setEncoding(out_token_id, "UTF-8");
  // setEncoding(out_form, "UTF-8");
  // setEncoding(out_lemma, "UTF-8");
  // setEncoding(out_upos, "UTF-8");
  // setEncoding(out_xpos, "UTF-8");
  // setEncoding(out_feats, "UTF-8");
  // setEncoding(out_dep_rel, "UTF-8");
  // setEncoding(out_misc, "UTF-8");

  for (int i = 0; i < x.size(); i++){
    if(log_every > 0){
      if (i % log_every == 0){
        Rcpp::Rcout << Rcpp::as<std::string>(current_time()) << " Annotating text fragment " << i + 1 << "/" << x.size() << std::endl;  
      }  
    }
    text   = Rcpp::as<std::string>(x[i]);
    doc_id = Rcpp::as<std::string>(docid[i]);
    languagemodel_pipeline.set_document_id(doc_id);
    ufal::udpipe::sentence s;
    reader->reset_document(doc_id);

    reader->set_text(text);
    int sentence_id = 0;
    while (reader->next_sentence(s, error)) {
      sentence_id = sentence_id + 1;
      if (languagemodel_pipeline.tagger != ufal::udpipe::pipeline::NONE){
        if (!languagemodel_pipeline.m->tag(s, languagemodel_pipeline.tagger, error)){
          Rcpp::stop(error);
        }
      }
      if (languagemodel_pipeline.parser != ufal::udpipe::pipeline::NONE){
        if (!languagemodel_pipeline.m->parse(s, languagemodel_pipeline.parser, error)){
          Rcpp::stop(error);
        }
      }
      //s.get_text(text);
      //Rcpp::Rcout <<  text << "-" << int(s.words.size()) << "-" << int(s.empty_nodes.size()) << "\n";
      size_t multiword_token = 0;
      for(int idx = 1; idx < ((int)(s.words.size())); idx++){
        //size_t token_start;
        //size_t token_end;
        //s.words[idx].get_token_range(token_start, token_end);
        //out_start.push_back((int)token_start);
        //out_end.push_back((int)token_end);
        if (multiword_token < s.multiword_tokens.size() && idx == s.multiword_tokens[multiword_token].id_first) {
          // add multi-word token: only token_id, form and misc known for multi-word tokens
          out_doc_id.push_back(doc_id);
          out_sentence_id.push_back(sentence_id);
          out_term_id.push_back(idx + multiword_token);
          out_token_id.push_back(std::to_string(s.multiword_tokens[multiword_token].id_first) + '-' + std::to_string(s.multiword_tokens[multiword_token].id_last));
          out_form.push_back(s.multiword_tokens[multiword_token].form);
          out_misc.push_back(s.multiword_tokens[multiword_token].misc);
          multiword_token++;
          out_lemma.push_back(NA_STRING);
          out_upos.push_back(NA_STRING);
          out_xpos.push_back(NA_STRING);  
          out_feats.push_back(NA_STRING); 
          out_head.push_back(NA_INTEGER); 
          out_dep_rel.push_back(NA_STRING); 
          //out_deps.push_back(NA_STRING); 
        }
        out_doc_id.push_back(doc_id);
        out_sentence_id.push_back(sentence_id);
        out_term_id.push_back(idx + multiword_token);
        out_token_id.push_back(std::to_string(s.words[idx].id));
        out_form.push_back(s.words[idx].form);
        if(!s.words[idx].lemma.empty()) out_lemma.push_back(s.words[idx].lemma); else out_lemma.push_back(NA_STRING);
        if(!s.words[idx].upostag.empty()) out_upos.push_back(s.words[idx].upostag); else out_upos.push_back(NA_STRING);
        if(!s.words[idx].xpostag.empty()) out_xpos.push_back(s.words[idx].xpostag); else out_xpos.push_back(NA_STRING);
        if(!s.words[idx].feats.empty()) out_feats.push_back(s.words[idx].feats); else out_feats.push_back(NA_STRING);
        if(s.words[idx].head >= 0) out_head.push_back(s.words[idx].head); else out_head.push_back(NA_INTEGER);
        if(!s.words[idx].deprel.empty()) out_dep_rel.push_back(s.words[idx].deprel); else out_dep_rel.push_back(NA_STRING);
        //if(!s.words[idx].deps.empty()) out_deps.push_back(s.words[idx].deps); else out_deps.push_back(NA_STRING);
        if(!s.words[idx].misc.empty()) out_misc.push_back(s.words[idx].misc); else out_misc.push_back(NA_STRING);
        //if(s.words[idx].id > 0){
        //Rcpp::Rcout << "sentence" << sentence_id << " word " << idx << "\n";
        //Rcpp::Rcout <<  idx << ": " << s.words[idx].lemma << "\n";
        //if(!s.words[idx].form.empty()) out_lemma.push_back(s.words[idx].form);
        //}
      }
    }
  }
  
  // Return
  Rcpp::List output = Rcpp::List::create(Rcpp::Named("doc_id") = out_doc_id,
                                         Rcpp::Named("sentence_id") = out_sentence_id, 
                                         //Rcpp::Named("start") = out_start,
                                         //Rcpp::Named("end") = out_end,
                                         Rcpp::Named("term_id") = out_term_id,
                                         Rcpp::Named("token_id") = out_token_id,
                                         Rcpp::Named("token") = out_form,
                                         Rcpp::Named("lemma") = out_lemma,
                                         Rcpp::Named("upos") = out_upos,
                                         Rcpp::Named("xpos") = out_xpos,
                                         Rcpp::Named("feats") = out_feats,
                                         Rcpp::Named("head_token_id") = out_head,
                                         Rcpp::Named("dep_rel") = out_dep_rel,
                                         //Rcpp::Named("deps") = out_deps,
                                         Rcpp::Named("misc") = out_misc);
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