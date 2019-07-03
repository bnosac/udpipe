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
x <- as.data.frame(x, detailed = TRUE)
x
```

Or just do as follows.

```
library(udpipe)
x <- udpipe(x = "Ik ging op reis en ik nam mee: mijn laptop, mijn zonnebril en goed humeur.", 
            object = "dutch")
```


The annotation returns paragraphs, sentences, tokens, the location of the token in the original text, morphology elements like the lemma, the universal part of speech tag and the treebank-specific parts of speech tag, morphosyntactic features and returns as well the dependency relationship. More information at http://universaldependencies.org/guidelines.html

```
 doc_id paragraph_id sentence_id start end term_id token_id     token     lemma  upos                                        xpos                               feats head_token_id      dep_rel            misc
   doc1            1           1     1   2       1        1        Ik        ik  PRON                VNW|pers|pron|nomin|vol|1|ev      Case=Nom|Person=1|PronType=Prs             2        nsubj            <NA>
   doc1            1           1     4   7       2        2      ging      gaan  VERB                               WW|pv|verl|ev Number=Sing|Tense=Past|VerbForm=Fin             0         root            <NA>
   doc1            1           1     9  10       3        3        op        op   ADP                                     VZ|init                                <NA>             4         case            <NA>
   doc1            1           1    12  15       4        4      reis      reis  NOUN                  N|soort|ev|basis|zijd|stan              Gender=Com|Number=Sing             2          obl            <NA>
   doc1            1           1    17  18       5        5        en        en CCONJ                                    VG|neven                                <NA>             7           cc            <NA>
   doc1            1           1    20  21       6        6        ik        ik  PRON                VNW|pers|pron|nomin|vol|1|ev      Case=Nom|Person=1|PronType=Prs             7        nsubj            <NA>
   doc1            1           1    23  25       7        7       nam     nemen  VERB                               WW|pv|verl|ev Number=Sing|Tense=Past|VerbForm=Fin             2         conj            <NA>
   doc1            1           1    27  29       8        8       mee       mee   ADP                                      VZ|fin                                <NA>             7 compound:prt   SpaceAfter=No
   doc1            1           1    30  30       9        9         :         : PUNCT                                         LET                                <NA>             7        punct            <NA>
...
```

## A small note on encodings

Mark that it is important that the `x` argument to `udpipe_annotate` is in UTF-8 encoding. 
You can check the encoding of your text with `Encoding('your text')`. You can convert your text to UTF-8, using standard R utilities: as in `iconv('your text', from = 'latin1', to = 'UTF-8')` where you replace the `from` part with whichever encoding you have your text in, possible your computers default as defined in `localeToCharset()`. So annotation would look something like this if your text is not already in UTF-8 encoding: 

- `udpipe_annotate(udmodel, x = iconv('your text', to = 'UTF-8'))` if your text is in the encoding of the current locale of your computer.
- `udpipe_annotate(udmodel, x = iconv('your text', from = 'latin1', to = 'UTF-8'))` if your text is in latin1 encoding. 
- `udpipe_annotate(udmodel, x = iconv('your text', from = 'CP949', to = 'UTF-8'))` if your text is in CP949 encoding. 
