# udpipe - R package for Tokenization, Tagging, Lemmatization and Dependency Parsing Based on UDPipe 

This repository contains an R package which is an Rcpp wrapper around the UDPipe C++ library (http://ufal.mff.cuni.cz/udpipe, https://github.com/ufal/udpipe).

UDPipe provides fast, accurate and language-agnostic tokenization, tagging, lemmatization and dependency parsing of raw text, which is an essential part in natural language processing.
The techniques used are explained in detail in the paper: "Tokenizing, POS Tagging, Lemmatizing and Parsing UD 2.0 with UDPipe", available at <http://ufal.mff.cuni.cz/~straka/papers/2017-conll_udpipe.pdf>.
    

## Usage

Currently the package allows you to do fast tokenisation, tagging, lemmatization and dependency parsing with one convenient function `udpipe_annotate`

```
library(udpipe)

## Load the model (see remark below where to get these models or look at ?udpipe_load_model)
f <- file.path(getwd(), "dev/udpipe-ud-2.0-170801/dutch-ud-2.0-170801.udpipe")
ud_dutch <- udpipe_load_model(f)

## Tokenise, Tag and Dependency Parsing Annotation. Output is in CONLL-U format.
txt <- c("Dus. Godvermehoeren met pus in alle puisten, 
  zei die schele van Van Bukburg en hij had nog gelijk ook. 
  Er was toen dat liedje van tietenkonttieten kont tieten kontkontkont, 
  maar dat hoefden we geenseens niet te zingen. 
  Je kunt zeggen wat je wil van al die gesluierde poezenpas maar d'r kwam wel 
  een vleeswarenwinkel onder te voorschijn van heb je me daar nou.
  
  En zo gaat het maar door.",
  "Wat die ransaap van een academici nou weer in z'n botte pan heb gehaald mag 
  Joost in m'n schoen gooien, maar feit staat boven water dat het een gore 
  vieze vuile ransaap is.")
x <- udpipe_annotate(ud_dutch, x = txt)
cat(x$conllu)
```

Pre-trained Universal Dependencies 2.0 models on all UD treebanks are made available at 
https://ufal.mff.cuni.cz/udpipe, namely at https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364.
At the time of writing this consists of models made available on 50 languages, namely: 
ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, czech, danish, dutch, english, estonian, finnish, french, galician, german, gothic, greek, hebrew, hindi, hungarian, indonesian, irish, italian, japanese, kazakh, korean, latin, latvian, lithuanian, norwegian, old_church_slavonic, persian, polish, portuguese, romanian, russian, sanskrit, slovak, slovenian, spanish, swedish, tamil, turkish, ukrainian, urdu, uyghur, vietnamese. 
Mark that these models are made available under the CC BY-NC-SA 4.0 license.


## Installation & LICENSE

The package is availabe under the Mozilla Public License Version 2.0. Installation can be done as follows and has only Rcpp as dependency.

```
install.packages("devtools")
devtools::install_github("bnosac/udpipe", build_vignettes = TRUE)
```

## Support in text mining

Need support in text mining?
Contact BNOSAC: http://www.bnosac.be

