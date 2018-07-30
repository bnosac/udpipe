# CHANGES IN udpipe VERSION 0.6.1

- src/udpipe.cpp: at the request of CRAN: remove dynamic execution specification which g++-7 and later complain about by removing the throw statements
- add ctb role to authors Milan and Jana in DESCRIPTION 

# CHANGES IN udpipe VERSION 0.6

- Added cbind_morphological and cbind_dependencies
- Allow to show progress in udpipe_annotate
- txt_nextgram now does not paste NA's together in case someone would use it with missing text data
- Add example on only doing pos tagging and dependency parsing and excluding tokenisation
- Fix gcc8 message: warning: 'char* strncpy(char*, const char*, size_t)' specified bound 15 equals destination size [-Wstringop-truncation]

# CHANGES IN udpipe VERSION 0.5

- Added txt_recode_ngram for recoding tokens with compound multi-word expressions
- Fix to make sure as.data.frame.udpipe_connlu also works with data.table version 1.9.6. Fixes issue #16
- Allow keywords_rake to use in group a character vector of column names
- Added a vignette on the use of the package to do topic modelling using the POS tags and multi-word expressions
- Add example of correlation analysis in vignette on 'Basic Analytical Use Cases'
- dtm_remove_lowfreq to uses minfreq as lower bound

# CHANGES IN udpipe VERSION 0.4

- Fix R CMD check on clang-UBSAN: UndefinedBehaviorSanitizer (runtime error: reference binding to misaligned address)
- Add more documentation on required UTF-8 encoding
- Add as_conllu
- Add as_word2vec
- Add as.data.table.udpipe_conllu for convenience
- Add keywords_rake and keywords_collocation
- Exported also keywords_collocation and keywords_phrases
- Add document_term_frequencies_statistics
- Add boilerplate functions dtm_rowsums and dtm_colsums
- Make output of keywords_collocation, keywords_rake and keywords_phrases consistent
- Allow cooccurrence.data.frame to provide a vector of groups
- Added another vignette

# CHANGES IN udpipe VERSION 0.3

- Add docusaurus site
- udpipe_download_model gains and extra argument called udpipe_model_repo to allow to download models mainly released under CC-BY-SA from https://github.com/bnosac/udpipe.models.ud
- Add udpipe_accuracy 
- Add dtm_rbind and dtm_cbind 
- Add udpipe_read_conllu to simplify creating wordvectors 
- Allow to provide several fields in document_term_frequencies to easily allow to include bigrams/trigrams/... for topic modelling purposes e.g. alongside the textrank package or alongside collocation
- Adding Serbian + Afrikaans
- Fixing UBSAN messages (misaligned addresses)
- If user has R version < 3.3.0, use own startsWith function instead of base::startsWith
 
# CHANGES IN udpipe VERSION 0.2.2

- Another stab at fixing the Solaris compilation issue in  ufal::udpipe::multiword_splitter::append_token

# CHANGES IN udpipe VERSION 0.2.1

- Added phrases to extract POS sequences more easily like noun phrases, verb phrases or any sequence of parts of speech tags and their corresponding words
- Fix issue in txt_nextgram if n was larger than the number of elements in x
- Fix heap-use-after-free address sanitiser issue
- Fix runtime error: null pointer passed as argument 1, which is declared to never be null (e.g. udpipe.cpp: 3338)
- Another stab at the Solaris compilation issue


# CHANGES IN udpipe VERSION 0.2

- Added data preparation elements for standard text mining flows namely: 
    cooccurrence
    collocation
    document_term_frequencies
    document_term_matrix
    dtm_tfidf
    dtm_remove_terms
    dtm_remove_lowfreq
    dtm_remove_tfidf
    dtm_reverse
    dtm_cor
    txt_collapse
    txt_sample
    txt_show
    txt_highlight
    txt_recode
    txt_previous
    txt_next
    txt_nextgram
    unique_identifier
- Added predict.LDA_VEM and predict.LDA_Gibbs
- Renamed dataset annotation_params to udpipe_annotation_params
- Added example datasets called brussels_listings, brussels_reviews, brussels_reviews_anno
- Use path.expand on conll-u files which are used for training
- udpipe_download_model now downloads from https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.0/master instead of https://github.com/jwijffels/udpipe.models.ud.2.0/raw/master

# CHANGES IN udpipe VERSION 0.1.2

- Remove logic of UDPIPE_PROCESS_LOG (using Rcpp::Rout instead). This fixes issue detected with valgrind about ofstream

# CHANGES IN udpipe VERSION 0.1.1

- Fix issue on Solaris builds at CRAN, namely: error: expected primary-expression before ‘enum’
- Use ufal::udpipe namespace directly
- Documentation fixes

# CHANGES IN udpipe VERSION 0.1

- Initial release based on UDPipe commit a2ebb99d243546f64c95d0faf36882bb1d67a670
- Allow to do annotation (tokenisation, POS tagging, Lemmatisation, Dependency parsing)
- Allow to build your own UDPipe model based on data in CONLL-U format
- Convert the output of udpipe_annotate to a data.frame
- Allow to download models from https://github.com/jwijffels/udpipe.models.ud.2.0
- Add vignettes

