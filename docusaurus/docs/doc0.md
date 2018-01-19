---
id: doc0
title: Try it out
sidebar_label: Try it out
---

Install the R package. 

```
install.packages("udpipe")
```

## Example

Get your language model and start annotating.

```
library(udpipe)
udmodel <- udpipe_download_model(language = "dutch")
udmodel <- udpipe_load_model(file = udmodel$file_model)
x <- udpipe_annotate(udmodel, x = "Ik ging op reis en ik nam mee: mijn laptop, mijn zonnebril en goed humeur.")
x <- as.data.frame(x)
x
```

The annotation returns paragraphs, sentences, tokens, morphology elements like the lemma, the universal part of speech tag and the treebank-specific parts of speech tag, morphosyntactic features and returns as well the dependency relationship. More information at http://universaldependencies.org/guidelines.html

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

## A small note on encodings

Mark that it is important that the `x` argument to `udpipe_annotate` is in UTF-8 encoding. You can check the encoding of your text with `Encoding('your text')` and to convert your text to UTF-8, use standard R utilities to convert to the right encoding 
as in `iconv('your text', from = 'latin1', to = 'UTF-8')` where you replace the from part with whichever encoding you have your text in, possible your computers default as defined in `localeToCharset()`. So annotation would look something like this: `udpipe_annotate(udmodel, x = iconv('your text', from = 'latin1', to = 'UTF-8'))` or like this `udpipe_annotate(udmodel, x = iconv('your text', to = 'UTF-8'))` if your text is in the encoding of the current locale of your computer.