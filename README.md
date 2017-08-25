# udpipe - R package for Tokenization, Tagging, Lemmatization and Dependency Parsing Based on UDPipe 

This repository contains an R package which is an Rcpp wrapper around the UDPipe C++ library (http://ufal.mff.cuni.cz/udpipe, https://github.com/ufal/udpipe).

- UDPipe provides fast, accurate and language-agnostic tokenization, tagging, lemmatization and dependency parsing of raw text, which is an essential part in natural language processing.
- The techniques used are explained in detail in the paper: "Tokenizing, POS Tagging, Lemmatizing and Parsing UD 2.0 with UDPipe", available at <http://ufal.mff.cuni.cz/~straka/papers/2017-conll_udpipe.pdf>.
- The R package only has Rcpp and data.table as dependency. No tidyverse if you don't want this!
    

## Usage

Currently the package allows you to do fast tokenisation, tagging, lemmatization and dependency parsing with one convenient function called. `udpipe_annotate`

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

The resulting tagged output is in CONLL-U format as described at http://universaldependencies.org/format.html.

```
# newdoc id = d1
# newpar
# sent_id = 1
# text = Dus.
1	Dus	dus	ADV	Adv|gew|aanw	PronType=Dem	0	root	_	SpaceAfter=No
2	.	.	PUNCT	Punc|punt	PunctType=Peri	1	punct	_	_

# sent_id = 2
# text = Godvermehoeren met pus in alle puisten, zei die schele van Van Bukburg en hij had nog gelijk ook.
1	Godvermehoeren	Godvermehoeer	VERB	V|intrans|inf	Subcat=Intr|VerbForm=Inf	0	root	_	_
2	met	met	ADP	Prep|voor	AdpType=Prep	3	case	_	_
3	pus	pus	NOUN	N|soort|mv|neut	Number=Plur	1	obl	_	_
4	in	in	ADP	Prep|voor	AdpType=Prep	6	case	_	_
5	alle	alle	PRON	Pron|onbep|neut|attr	PronType=Ind	6	nmod	_	_
6	puisten	puist	NOUN	N|soort|mv|neut	Number=Plur	1	obl	_	SpaceAfter=No
7	,	,	PUNCT	Punc|komma	PunctType=Comm	1	punct	_	SpacesAfter=\s\n\s\s
8	zei	zeg	VERB
...
```

You can also get this in a tidy data.frame format with the function as.data.frame.
This uses the data.table package.

```
as.data.frame(x)

 doc_id paragraph_id sentence_id id           form         lemma upostag              xpostag                    feats head deprel deps          misc
     d1            1           1  1            Dus           dus     ADV         Adv|gew|aanw             PronType=Dem    0   root    _ SpaceAfter=No
     d1            1           1  2              .             .   PUNCT            Punc|punt           PunctType=Peri    1  punct    _             _
     d1            1           2  1 Godvermehoeren Godvermehoeer    VERB        V|intrans|inf Subcat=Intr|VerbForm=Inf    0   root    _             _
     d1            1           2  2            met           met     ADP            Prep|voor             AdpType=Prep    3   case    _             _
     d1            1           2  3            pus           pus    NOUN      N|soort|mv|neut              Number=Plur    1    obl    _             _
     d1            1           2  4             in            in     ADP            Prep|voor             AdpType=Prep    6   case    _             _
     d1            1           2  5           alle          alle    PRON Pron|onbep|neut|attr             PronType=Ind    6   nmod    _             _
...
```

Pre-trained Universal Dependencies 2.0 models on all UD treebanks are made available at 
https://ufal.mff.cuni.cz/udpipe, namely at https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364.

At the time of writing this consists of models made available on 50 languages, namely: 
ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, czech, danish, dutch, english, estonian, finnish, french, galician, german, gothic, greek, hebrew, hindi, hungarian, indonesian, irish, italian, japanese, kazakh, korean, latin, latvian, lithuanian, norwegian, old_church_slavonic, persian, polish, portuguese, romanian, russian, sanskrit, slovak, slovenian, spanish, swedish, tamil, turkish, ukrainian, urdu, uyghur, vietnamese. 

Mark that these models are made available under the CC BY-NC-SA 4.0 license.


## Installation & License

The package is availabe under the Mozilla Public License Version 2.0.
Installation can be done as follows.

```
install.packages("devtools")
devtools::install_github("bnosac/udpipe", build_vignettes = TRUE)
```

## Support in text mining

Need support in text mining?
Contact BNOSAC: http://www.bnosac.be

