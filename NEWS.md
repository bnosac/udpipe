## CHANGES IN udpipe VERSION 0.8.15

- Drop C++11 from Makevars

## CHANGES IN udpipe VERSION 0.8.14

- Add comment section in Authors@R and put aut instead of ctb for udpipe.cpp part

## CHANGES IN udpipe VERSION 0.8.13

- fix load of misaligned address and UBSan messages reported by CRAN

## CHANGES IN udpipe VERSION 0.8.12

- avoid warning: overlapping comparisons always evaluate to true in parse_int 
   - replacing: !(str.str[0] >= '0' || str.str[0] <= '9') with (str.str[0] < '0' || str.str[0] > '9')
- fix some R CMD check NOTEs about the use of itemize in the documentation
- fix a vignette index entry NOTE
- fix of an URL in the documentation
   
## CHANGES IN udpipe VERSION 0.8.11

- replace move with std::move to fix R CMD check warning on recent versions of clang compilers

## CHANGES IN udpipe VERSION 0.8.10

- use snprintf instead of sprintf to handle the R CMD check deprecating note on M1mac
- reduction of timings of the examples of document_term_matrix, document_term_frequencies, document_term_frequencies_statistics, cooccurrence, dtm_bind, keywords_collocation

## CHANGES IN udpipe VERSION 0.8.9

- fix R CMD check message on Fedora clang infrastructure: rcpp_udpipe.cpp:243:8: warning: use of bitwise '&' with boolean operands

## CHANGES IN udpipe VERSION 0.8.8

- dtm_svd_similarity, fix to make sure that if provided a dtm with features which are all missing/zero, 
  the scoring still works as expected instead of removing features which contain no data whatsoever.
  So that dtm_svd_similarity can be used alongside embeddings of R package word2vec which might contain words which are not in the dtm. See the example in ?dtm_svd_similarity
- added txt_grepl
- dtm_align now uses NCOL to see if y is a vector instead of a data.frame

## CHANGES IN udpipe VERSION 0.8.7

- txt_count now always returns an integer, even if in the border case where a character vector of length 0 is supplied

## CHANGES IN udpipe VERSION 0.8.6

- Downloading models to paths containing non-ASCII characters now works (issue #95)
- strsplit.data.frame gains ... which are passed on to strsplit (e.g. to use fixed=TRUE for speeding up)
- read_connlu is now using fixed=TRUE when splitting by newline symbol (for speeding up parsing with function udpipe)
- Added txt_paste
- Added txt_context
- Use html_vignette instead of html_document in the vignettes in order to reduce package size

## CHANGES IN udpipe VERSION 0.8.5

- Added document_term_matrix.default, document_term_matrix.integer and document_term_matrix.numeric
- Added groups argument to dtm_colsums and dtm_rowsums
- Added dtm_align
- Added dtm_sample
- Added document_term_matrix.matrix
- dtm_cbind and dtm_rbind allow to pass more than 2 sparse matrices
- cbind_morphological gains argument which to specify which morphological features to extract
- txt_count now returns NA when NA is provided instead of an error
- txt_contains now returns NA when NA is provided instead of FALSE, unless value is set to TRUE
- txt_collapse now also works if provided a list of character vectors
- paste.data.frame now works as well if a data.table is passed instead of a data.frame
- txt_recode gains an extra argument na.rm

## CHANGES IN udpipe VERSION 0.8.4-1

- Fixing the Solaris compilation issue in ufal::udpipe::multiword_splitter::append_token

## CHANGES IN udpipe VERSION 0.8.4

- Update to UDPipe 1.2.1 (28 Sep 2018) 
    - this adds segment_size and learning_rate_final parameters to tokenizer training
    - correctly set SpaceAfter for last token when normalizing spaces.
- Default of udpipe_download_model is now changed, downloads now models built on Universal Dependencies 2.5 instead of the models build on Universal Dependencies 2.4
- Added txt_count
- Added txt_overlap
- Added dtm_conform
- Added dtm_chisq
- Added dtm_svd_similarity
- Added as_fasttext
- Added unlist_tokens
- txt_recode_ngram now also works gracefully in case ngram is set to 1 although the intention is not to use it when ngram is set to 1
- Experimental changes regarding cbind_dependencies which might change in a subsequent release.
  - cbind_dependencies now has been implementend for type 'child'. 
  - cbind_dependencies now allows to add row numbers of the parent or children where the token is linked to using the dependency parsing output. 
- Experimental and unfinished work on allowing to easily query dependency relations


## CHANGES IN udpipe VERSION 0.8.3

- Default of udpipe_download_model is now changed, downloads now models built on Universal Dependencies 2.4 instead of the models build on Universal Dependencies 2.3
- also allow strsplit.data.frame to work if the data argument is a data.table
- in case the model loaded with udpipe_load_model is a nil pointer (most likely due to users which restarted their R sessions without knowing), try reloading the model file in udpipe_annotate
- fix issue in udpipe_reconstruct giving wrong values in start/end positions of the token in case someone had as well SpacesBefore as SpacesAfter for a token. For users prior to version 0.8.3 you can easily circumvent this issue by removing leading/trailing white space in your text by using trimws on your text before using udpipe::udpipe.
- document_term_matrix now gains argument weight allowing to select another column to put into the matrix cells
- add txt_contains

## CHANGES IN udpipe VERSION 0.8.2

- udpipe::udpipe now gains 2 arguments: parallel.cores and parallel.chunksize in order to annotate in parallel over your CPU cores.
- document_term_matrix.data.frame now preserves order of the documents (issue #44)
- dtm_remove_lowfreq, dtm_remove_tfidf, dtm_remove_terms gain extra argument remove_emptydocs
  explicitely add drop=FALSE to internal dtm_... calls
- add dtm_remove_sparseterms (issue #44)
- make sure downloading model fails gracefully if github internet resource is not available on CRAN machines
- udpipe_download_model now also returns download_failed/download_message indicating if the download failed due to internet connectivity issues

## CHANGES IN udpipe VERSION 0.8.1

- Allow to pass on a .udpipe filename in udpipe_download_model
- Update documentation on keywords_collocation
- Added strsplit.data.frame and paste.data.frame

## CHANGES IN udpipe VERSION 0.8

- Default of udpipe_download_model is now changed, downloads now models built on Universal Dependencies 2.3 instead of the models build on Universal Dependencies 2.0
- Incorporate models from Universal Dependencies 2.3 released on 2018-11-15
- Incorporate models from conll18 shared task baseline built on Universal Dependencies 2.2
- In case someone uses document_term_frequencies.character incorrectly with double document identifiers, make sure this is handled
- txt_recode now returns x if the length of x is 0
- added txt_sentiment
- added txt_previousgram

## CHANGES IN udpipe VERSION 0.7

- Allow to reconstruct the original text + allow to add a start/end field in as.data.frame (useful but undocumented feature). Set up mainly to be used with the crfsuite R package
- Added txt_tagsequence
- Added 1 general function called udpipe which does annotation of data in TIF format.
- Add option in udpipe_download_model to download the model only it does not exist on disk
- Loaded model are put into an environment such that users of the function udpipe do not need to care about loading

## CHANGES IN udpipe VERSION 0.6.1

- src/udpipe.cpp: at the request of CRAN: remove dynamic execution specification which g++-7 and later complain about by removing the throw statements
- add ctb role to authors Milan and Jana in DESCRIPTION 

## CHANGES IN udpipe VERSION 0.6

- Added cbind_morphological and cbind_dependencies
- Allow to show progress in udpipe_annotate
- txt_nextgram now does not paste NA's together in case someone would use it with missing text data
- Add example on only doing pos tagging and dependency parsing and excluding tokenisation
- Fix gcc8 message: warning: 'char* strncpy(char*, const char*, size_t)' specified bound 15 equals destination size [-Wstringop-truncation]

## CHANGES IN udpipe VERSION 0.5

- Added txt_recode_ngram for recoding tokens with compound multi-word expressions
- Fix to make sure as.data.frame.udpipe_connlu also works with data.table version 1.9.6. Fixes issue #16
- Allow keywords_rake to use in group a character vector of column names
- Added a vignette on the use of the package to do topic modelling using the POS tags and multi-word expressions
- Add example of correlation analysis in vignette on 'Basic Analytical Use Cases'
- dtm_remove_lowfreq to uses minfreq as lower bound

## CHANGES IN udpipe VERSION 0.4

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

## CHANGES IN udpipe VERSION 0.3

- Add docusaurus site
- udpipe_download_model gains and extra argument called udpipe_model_repo to allow to download models mainly released under CC-BY-SA from https://github.com/bnosac/udpipe.models.ud
- Add udpipe_accuracy 
- Add dtm_rbind and dtm_cbind 
- Add udpipe_read_conllu to simplify creating wordvectors 
- Allow to provide several fields in document_term_frequencies to easily allow to include bigrams/trigrams/... for topic modelling purposes e.g. alongside the textrank package or alongside collocation
- Adding Serbian + Afrikaans
- Fixing UBSAN messages (misaligned addresses)
- If user has R version < 3.3.0, use own startsWith function instead of base::startsWith
 
## CHANGES IN udpipe VERSION 0.2.2

- Another stab at fixing the Solaris compilation issue in  ufal::udpipe::multiword_splitter::append_token

## CHANGES IN udpipe VERSION 0.2.1

- Added phrases to extract POS sequences more easily like noun phrases, verb phrases or any sequence of parts of speech tags and their corresponding words
- Fix issue in txt_nextgram if n was larger than the number of elements in x
- Fix heap-use-after-free address sanitiser issue
- Fix runtime error: null pointer passed as argument 1, which is declared to never be null (e.g. udpipe.cpp: 3338)
- Another stab at the Solaris compilation issue


## CHANGES IN udpipe VERSION 0.2

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

## CHANGES IN udpipe VERSION 0.1.2

- Remove logic of UDPIPE_PROCESS_LOG (using Rcpp::Rout instead). This fixes issue detected with valgrind about ofstream

## CHANGES IN udpipe VERSION 0.1.1

- Fix issue on Solaris builds at CRAN, namely: error: expected primary-expression before ‘enum’
- Use ufal::udpipe namespace directly
- Documentation fixes

## CHANGES IN udpipe VERSION 0.1

- Initial release based on UDPipe commit a2ebb99d243546f64c95d0faf36882bb1d67a670
- Allow to do annotation (tokenisation, POS tagging, Lemmatisation, Dependency parsing)
- Allow to build your own UDPipe model based on data in CONLL-U format
- Convert the output of udpipe_annotate to a data.frame
- Allow to download models from https://github.com/jwijffels/udpipe.models.ud.2.0
- Add vignettes

