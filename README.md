# udpipe - R package for Tokenization, Tagging, Lemmatization and Dependency Parsing Based on UDPipe 

This repository contains an R package which is an Rcpp wrapper around the UDPipe C++ library (http://ufal.mff.cuni.cz/udpipe, https://github.com/ufal/udpipe).

- UDPipe provides language-agnostic tokenization, tagging, lemmatization and dependency parsing of raw text, which is an essential part in natural language processing.
- The techniques used are explained in detail in the paper: "Tokenizing, POS Tagging, Lemmatizing and Parsing UD 2.0 with UDPipe", available at <http://ufal.mff.cuni.cz/~straka/papers/2017-conll_udpipe.pdf>. In that paper, you'll also find accuracies on different languages and process flow speed (measured in words per second).

![](vignettes/udpipe-rlogo.png)

## General

The udpipe R package was designed with the following things in mind when building the Rcpp wrapper around the UDPipe C++ library:

- Give R users simple access in order to easily tokenize, tag, lemmatize or perform dependency parsing on text in any language
- Provide easy access to pre-trained annotation models
- Allow R users to easily construct your own annotation model based on data in CONLL-U format as provided in more than 60 treebanks available at http://universaldependencies.org/#ud-treebanks
- Don't rely on Python or Java so that R users can easily install this package without configuration hassle
- No external R package dependencies except the strict necessary (Rcpp and data.table, no tidyverse)

## Installation & License

The package is available under the Mozilla Public License Version 2.0.
Installation can be done as follows. Please visit the package documentation at https://bnosac.github.io/udpipe/en and look at the R package vignettes for further details.

```
install.packages("udpipe")
vignette("udpipe-tryitout", package = "udpipe")
vignette("udpipe-annotation", package = "udpipe")
vignette("udpipe-train", package = "udpipe")
vignette("udpipe-usecase-postagging-lemmatisation", package = "udpipe")
vignette("udpipe-usecase-topicmodelling", package = "udpipe")
```

For installing the development version of this package: `devtools::install_github("bnosac/udpipe", build_vignettes = TRUE)`

## Example

Currently the package allows you to do tokenisation, tagging, lemmatization and dependency parsing with one convenient function called `udpipe_annotate`

```
library(udpipe)
dl <- udpipe_download_model(language = "dutch")
dl

language                                                                      file_model
   dutch C:/Users/Jan/Dropbox/Work/RForgeBNOSAC/BNOSAC/udpipe/dutch-ud-2.0-170801.udpipe

udmodel_dutch <- udpipe_load_model(file = "dutch-ud-2.0-170801.udpipe")
x <- udpipe_annotate(udmodel_dutch, 
                     x = "Ik ging op reis en ik nam mee: mijn laptop, mijn zonnebril en goed humeur.")
x <- as.data.frame(x)
x
```

```
 doc_id paragraph_id sentence_id token_id token lemma  upos                     xpos                                                               feats head_token_id dep_rel deps
   doc1            1           1        1    Ik    ik  PRON        Pron|per|1|ev|nom                          Case=Nom|Number=Sing|Person=1|PronType=Prs             2   nsubj <NA>
   doc1            1           1        2  ging    ga  VERB V|intrans|ovt|1of2of3|ev Aspect=Imp|Mood=Ind|Number=Sing|Subcat=Intr|Tense=Past|VerbForm=Fin             0    root <NA>
   doc1            1           1        3    op    op   ADP                Prep|voor                                                        AdpType=Prep             4    case <NA>
   doc1            1           1        4  reis  reis  NOUN          N|soort|ev|neut                                                         Number=Sing             2     obj <NA>
   doc1            1           1        5    en    en CCONJ               Conj|neven                                                                <NA>             7      cc <NA>
   doc1            1           1        6    ik    ik  PRON        Pron|per|1|ev|nom                          Case=Nom|Number=Sing|Person=1|PronType=Prs             7   nsubj <NA>
   doc1            1           1        7   nam  neem  VERB   V|trans|ovt|1of2of3|ev Aspect=Imp|Mood=Ind|Number=Sing|Subcat=Tran|Tense=Past|VerbForm=Fin             2    conj <NA>
...
```


## Pre-trained models

Pre-trained Universal Dependencies 2.0 models on all UD treebanks are made available for more than 50 languages, namely:

afrikaans, ancient_greek-proiel, ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, czech-cac, czech-cltt, czech, danish, dutch-lassysmall, dutch, english-lines, english-partut, english, estonian, finnish-ftb, finnish, french-partut, french-sequoia, french, galician-treegal, galician, german, gothic, greek, hebrew, hindi, hungarian, indonesian, irish, italian, japanese, kazakh, korean, latin-ittb, latin-proiel, latin, latvian, lithuanian, norwegian-bokmaal, norwegian-nynorsk, old_church_slavonic, persian, polish, portuguese-br, portuguese, romanian, russian-syntagrus, russian, sanskrit, serbian, slovak, slovenian-sst, slovenian, spanish-ancora, spanish, swedish-lines, swedish, tamil, turkish, ukrainian, urdu, uyghur, vietnamese. 

These have been made available easily to users of the package by using `udpipe_download_model`

### How good are these models? 

- Accuracy statistics of models provided by the UDPipe authors which you download with udpipe_download_model from the default repository are available at [this link](https://github.com/jwijffels/udpipe.models.ud.2.0/blob/master/inst/udpipe-ud-2.0-170801/README).
- Accuracy statistics of models trained using this R package which you download with udpipe_download_model from the bnosac/udpipe.models.ud repository are available at https://github.com/bnosac/udpipe.models.ud.
- For a comparison between UDPipe and spaCy visit https://github.com/jwijffels/udpipe-spacy-comparison

## Train your own models based on CONLL-U data

The package also allows you to build your own annotation model. For this, you need to provide data in CONLL-U format.
These are provided for many languages at http://universaldependencies.org/#ud-treebanks, mostly under the CC-BY-SA license.
How this is done is detailed in the package vignette.

```
vignette("udpipe-train", package = "udpipe")
```


## Support in text mining

Need support in text mining?
Contact BNOSAC: http://www.bnosac.be

