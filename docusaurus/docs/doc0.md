---
id: doc0
title: Try it out
sidebar_label: Try it out
---

Install the R package. 

```
install.packages("udpipe")
```

Get your language model and start annotating.

```
library(udpipe)
udmodel_dutch <- udpipe_download_model(language = "dutch")
udmodel_dutch <- udpipe_load_model(file = udmodel_dutch$file_model)
x <- udpipe_annotate(udmodel_dutch, x = "Ik ging op reis en ik nam mee: mijn laptop, mijn zonnebril en goed humeur.")
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
