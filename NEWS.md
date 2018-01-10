# CHANGES IN udpipe VERSION 0.3

- Add udpipe_accuracy 
- Add dtm_rbind and dtm_cbind 
- Add udpipe_read_conllu to simplify creating wordvectors 
- Allow to provide several fields in document_term_frequencies to easily allow to include bigrams/trigrams/... for topic modelling purposes e.g. alongside the textrank package or alongside collocation
- udpipe_download_model gains and extra argument called udpipe_model_repo to allow to download models mainly released under CC-BY-SA from https://github.com/bnosac/udpipe.models.ud
- Adding Serbian + Afrikaans
- Fixing UBSAN messages (misaligned addresses)
 
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

