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

