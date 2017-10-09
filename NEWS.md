# CHANGES IN udpipe VERSION 0.2

- Added data preparation elements for standard text mining flows namely: 
    cooccurrence
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
    txt_recode
    txt_previous
    txt_next
    txt_nextgram
- Added predict.LDA_VEM and predict.LDA_Gibbs
- Renamed dataset annotation_params to udpipe_annotation_params
- Added example datasets called brussels_listings, brussels_reviews, brussels_reviews_anno

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

