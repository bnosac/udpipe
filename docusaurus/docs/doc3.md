---
id: doc3
title: Build models
sidebar_label: Model building
---

## General

This package vignette shows how to build your own text annotation models based on UDPipe, allowing you to have full control over how you like that the model will execute: Tokenization (1), Parts of Speech tagging (2), Lemmatization (3) and Dependency Parsing (4). 

This section is also relevant if you work in a commercial setting where you would like to build and use your own models to annotate text. Note that some pre-trained models which you can download with `udpipe_download_model` were released under the CC-BY-NC-SA license, others were released under the CC-BY-SA license, the latter allowing for more liberal use. 

In order to train annotation models, you need to have data in **CONLL-U format**, a format which is described at http://universaldependencies.org/format.html. At the time of writing this, for more than 60 languages, open treebanks in CONLL-U format are made available for download at http://universaldependencies.org/#ud-treebanks. Most of these treebanks are distributed under the CC-BY-SA license which allows commercial use.

Mark that if you will build your own models, you will probably be interested in reading the paper with the details of the techniques used by UDPipe: "Tokenizing, POS Tagging, Lemmatizing and Parsing UD 2.0 with UDPipe", available at <http://ufal.mff.cuni.cz/~straka/papers/2017-conll_udpipe.pdf>. 


## Model building

### Basic example

Currently the package allows you to fit a text annotation model by using the function `udpipe_train`. You have to give it a character vector of files which are in CONLL-U format (which you might have downloaded at http://universaldependencies.org/#ud-treebanks).

Such at file basically looks like this, or has a similar format. You can just download these from http://universaldependencies.org for the language of your choice.


```r
file_conllu <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
file_conllu
```

```
[1] "C:/Users/Jan/Documents/R/win-library/3.4/udpipe/dummydata/traindata.conllu"
```

```r
cat(head(readLines(file_conllu), 3), sep="\n")
```

```
# newdoc id = doc1
# newpar
# sent_id = 1
```

If you have this type of file and you provide it to `udpipe_train`, a model is saved on disk in a binary format which can then be used to annotate your text data using `udpipe_annotate` in order to Tokenize, get Parts of Speech tags, find Lemma's or to extract Dependency relationships.
Let's show how this training works on the toy CONLL-U file we just showed:



```r
library(udpipe)
m <- udpipe_train(file = "toymodel.udpipe", files_conllu_training = file_conllu, 
                  annotation_tokenizer = list(dimension = 16, 
                                              epochs = 1, 
                                              batch_size = 100, 
                                              dropout = 0.7),
                  annotation_tagger = list(iterations = 1, 
                                           models = 1, 
                                           provide_xpostag = 1, 
                                           provide_lemma = 0, 
                                           provide_feats = 0), 
                  annotation_parser = "none")
```

```
Training tokenizer with the following options: tokenize_url=1, allow_spaces=0, dimension=16
  epochs=1, batch_size=100, learning_rate=0.0050, dropout=0.7000, early_stopping=0
Epoch 1, logprob: -2.1721e+005, training acc: 84.20%
Tagger model 1 columns: lemma use=1/provide=0, xpostag use=1/provide=1, feats use=1/provide=0
Creating morphological dictionary for tagger model 1.
Tagger model 1 dictionary options: max_form_analyses=0, custom dictionary_file=none
Tagger model 1 guesser options: suffix_rules=8, prefixes_max=0, prefix_min_count=10, enrich_dictionary=6
Tagger model 1 options: iterations=1, early_stopping=0, templates=tagger
Training tagger model 1.
Iteration 1: done, accuracy 44.44%
```

```r
m$file_model
```

```
[1] "toymodel.udpipe"
```

```r
## The model is now trained and saved in file toymodel.udpipe in the current working directory
## Now we can use the model to annotate some text
mymodel <- udpipe_load_model("toymodel.udpipe")
x <- udpipe_annotate(
  object = mymodel, 
  x = "Dit is een tokenizer met POS tagging, 
       zonder lemmatisation noch laat deze dependency parsing toe.", 
  parser = "none")
str(as.data.frame(x))
```

```
'data.frame':	15 obs. of  14 variables:
 $ doc_id       : chr  "doc1" "doc1" "doc1" "doc1" ...
 $ paragraph_id : int  1 1 1 1 1 1 1 1 1 1 ...
 $ sentence_id  : int  1 1 1 1 1 1 1 1 1 1 ...
 $ sentence     : chr  "Dit is een tokenizer met POS tagging, zonder lemmatisation noch laat deze dependency parsing toe." "Dit is een tokenizer met POS tagging, zonder lemmatisation noch laat deze dependency parsing toe." "Dit is een tokenizer met POS tagging, zonder lemmatisation noch laat deze dependency parsing toe." "Dit is een tokenizer met POS tagging, zonder lemmatisation noch laat deze dependency parsing toe." ...
 $ token_id     : chr  "1" "2" "3" "4" ...
 $ token        : chr  "Dit" "is" "een" "tokenizer" ...
 $ lemma        : chr  NA NA NA NA ...
 $ upos         : chr  "PRON" "VERB" "AUX" "NOUN" ...
 $ xpos         : chr  "Pron|onbep|neut|zelfst" "V|intrans|ott|3|ev" "V|hulpofkopp|ott|1|ev" "N|soort|ev|neut" ...
 $ feats        : chr  NA NA NA NA ...
 $ head_token_id: chr  NA NA NA NA ...
 $ dep_rel      : chr  NA NA NA NA ...
 $ deps         : chr  NA NA NA NA ...
 $ misc         : chr  NA NA NA NA ...
```

In the above example, we trained only a tokenizer and POS tagger, excluding lemmatisation and feature tagging and without dependency parsing. This was done by setting the `annotation_parser` argument to 'none' and setting `provide_lemma` and `provide_feats` to 0. The other arguments were merely set to reduce computation time in this package vignette. 

### Providing more details on the model annotation process

If you want to create a tagger which is capable of executing tokenisation, tagging as well as dependency parsing with the default settings of the algorithm, you just proceed as follows.


```r
m <- udpipe_train(file = "toymodel.udpipe", files_conllu_training = file_conllu, 
                  annotation_tokenizer = "default",
                  annotation_tagger = "default",
                  annotation_parser = "default")
```

When you want to train the model with specific tokenizer/tagger/parser settings, you need to provide these settings as a list to the respective arguments `annotation_tokenizer`, `annotation_tagger` and `annotation_parser`. The possible options for each of these settings are explained in detail below and their logic is detailed in the paper "Tokenizing, POS Tagging, Lemmatizing and Parsing UD 2.0 with UDPipe", available at <http://ufal.mff.cuni.cz/~straka/papers/2017-conll_udpipe.pdf>.


```r
params <- list()

## Tokenizer training parameters
params$tokenizer <- list(dimension = 24, 
                         epochs = 1, #epochs = 100, 
                         initialization_range = 0.1, 
                         batch_size = 100, learning_rate = 0.005, 
                         dropout = 0.1, early_stopping = 1)

## Tagger training parameters
params$tagger <- list(models = 2, 
  templates_1 = "tagger", 
      guesser_suffix_rules_1 = 8, guesser_enrich_dictionary_1 = 6, 
      guesser_prefixes_max_1 = 0, 
      use_lemma_1 = 0, use_xpostag_1 = 1, use_feats_1 = 1, 
      provide_lemma_1 = 0, provide_xpostag_1 = 1, 
      provide_feats_1 = 1, prune_features_1 = 0, 
  templates_2 = "lemmatizer", 
      guesser_suffix_rules_2 = 6, guesser_enrich_dictionary_2 = 4, 
      guesser_prefixes_max_2 = 4, 
      use_lemma_2 = 1, use_xpostag_2 = 0, use_feats_2 = 0, 
      provide_lemma_2 = 1, provide_xpostag_2 = 0, 
      provide_feats_2 = 0, prune_features_2 = 0)

## Dependency parser training parameters
params$parser <- list(iterations = 1, 
  #iterations = 30, 
  embedding_upostag = 20, embedding_feats = 20, embedding_xpostag = 0, 
  embedding_form = 50, 
  #embedding_form_file = "../ud-2.0-embeddings/nl.skip.forms.50.vectors", 
  embedding_lemma = 0, embedding_deprel = 20, 
  learning_rate = 0.01, learning_rate_final = 0.001, l2 = 0.5, hidden_layer = 200, 
  batch_size = 10, transition_system = "projective", transition_oracle = "dynamic", 
  structured_interval = 10)

## Train the model
m <- udpipe_train(file = "toymodel.udpipe", 
                  files_conllu_training = file_conllu, 
                  annotation_tokenizer = params$tokenizer,
                  annotation_tagger = params$tagger,
                  annotation_parser = params$parser)
```

```
Training tokenizer with the following options: tokenize_url=1, allow_spaces=0, dimension=24
  epochs=1, batch_size=100, learning_rate=0.0050, dropout=0.1000, early_stopping=1
Epoch 1, logprob: -1.4653e+005, training acc: 89.57%
Tagger model 1 columns: lemma use=0/provide=0, xpostag use=1/provide=1, feats use=1/provide=1
Creating morphological dictionary for tagger model 1.
Tagger model 1 dictionary options: max_form_analyses=0, custom dictionary_file=none
Tagger model 1 guesser options: suffix_rules=8, prefixes_max=0, prefix_min_count=10, enrich_dictionary=6
Tagger model 1 options: iterations=20, early_stopping=0, templates=tagger
Training tagger model 1.
Iteration 1: done, accuracy 37.04%
Iteration 2: done, accuracy 81.48%
Iteration 3: done, accuracy 100.00%
Iteration 4: done, accuracy 100.00%
Iteration 5: done, accuracy 100.00%
Iteration 6: done, accuracy 100.00%
Iteration 7: done, accuracy 100.00%
Iteration 8: done, accuracy 100.00%
Iteration 9: done, accuracy 100.00%
Iteration 10: done, accuracy 100.00%
Iteration 11: done, accuracy 100.00%
Iteration 12: done, accuracy 100.00%
Iteration 13: done, accuracy 100.00%
Iteration 14: done, accuracy 100.00%
Iteration 15: done, accuracy 100.00%
Iteration 16: done, accuracy 100.00%
Iteration 17: done, accuracy 100.00%
Iteration 18: done, accuracy 100.00%
Iteration 19: done, accuracy 100.00%
Iteration 20: done, accuracy 100.00%
Tagger model 2 columns: lemma use=1/provide=1, xpostag use=0/provide=0, feats use=0/provide=0
Creating morphological dictionary for tagger model 2.
Tagger model 2 dictionary options: max_form_analyses=0, custom dictionary_file=none
Tagger model 2 guesser options: suffix_rules=6, prefixes_max=4, prefix_min_count=10, enrich_dictionary=4
Tagger model 2 options: iterations=20, early_stopping=0, templates=lemmatizer
Training tagger model 2.
Iteration 1: done, accuracy 48.15%
Iteration 2: done, accuracy 77.78%
Iteration 3: done, accuracy 100.00%
Iteration 4: done, accuracy 100.00%
Iteration 5: done, accuracy 100.00%
Iteration 6: done, accuracy 100.00%
Iteration 7: done, accuracy 100.00%
Iteration 8: done, accuracy 100.00%
Iteration 9: done, accuracy 100.00%
Iteration 10: done, accuracy 100.00%
Iteration 11: done, accuracy 100.00%
Iteration 12: done, accuracy 100.00%
Iteration 13: done, accuracy 100.00%
Iteration 14: done, accuracy 100.00%
Iteration 15: done, accuracy 100.00%
Iteration 16: done, accuracy 100.00%
Iteration 17: done, accuracy 100.00%
Iteration 18: done, accuracy 100.00%
Iteration 19: done, accuracy 100.00%
Iteration 20: done, accuracy 100.00%
Parser transition options: system=projective, oracle=dynamic, structured_interval=10, single_root=1
Parser uses lemmas/upos/xpos/feats: automatically generated by tagger
Parser embeddings options: upostag=20, feats=20, xpostag=0, form=50, lemma=0, deprel=20
  form mincount=2, precomputed form embeddings=none
  lemma mincount=2, precomputed lemma embeddings=none
Parser network options: iterations=1, hidden_layer=200, batch_size=10,
  learning_rate=0.0100, learning_rate_final=0.0010, l2=0.5000, early_stopping=0
Initialized 'universal_tag' embedding with 0,9 words and 0.0%,100.0% coverage.
Initialized 'feats' embedding with 0,17 words and 0.0%,100.0% coverage.
Initialized 'form' embedding with 0,4 words and 0.0%,29.6% coverage.
Initialized 'deprel' embedding with 0,16 words and 0.0%,100.0% coverage.
Iteration 1: training logprob -1.8848e+002
```

As you have seen above in the example, if you want to train the dependency parser, you can also provide pre-trained word embeddings which you can provide in the `embedding_form_file` argument. Example training data can be found at https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364. If you also have a holdout file in CONLL-U format which you can provide in the `files_conllu_holdout` argument, the training is stopped before model performance decreases on the holdout CONLL-U file.


Mark. Before you embark in starting to train your own models with more realistic learning parameters, consider that training can take a while. 

## Settings for the tokenizer:

The tokenizer recognizes the following options:

- `tokenize_url` (default 1): tokenize URLs and emails using a manually implemented recognizer
- `allow_spaces` (default 1 if any token contains a space, 0 otherwise): allow tokens to contain spaces
- `dimension` (default 24): dimension of character embeddings and of the per-character bidirectional GRU. Note that inference time is quadratic in this parameter. Supported values are only 16, 24 and 64, with 64 needed only for languages with complicated tokenization like Japanese, Chinese or Vietnamese.
- `epochs` (default 100): the number of epochs to train the tokenizer for
- `batch_size` (default 50): batch size used during tokenizer training
- `learning_rate` (default 0.005): the learning rate used during tokenizer training
- `dropout` (default 0.1): dropout used during tokenizer training
- `early_stopping` (default 1 if heldout is given, 0 otherwise): perform early stopping, choosing training iteration maximizing sentences F1 score plus tokens F1 score on heldout data

During random hyperparameter search, `batch_size` is chosen uniformly from {50,100} and `learning_rate` logarithmically from <0.0005, 0.01). 

The tokenizer is trained using the SpaceAfter=No features in the CoNLL-U files. If the feature is not present, a detokenizer can be used to guess the SpaceAfter=No features according to a supplied plain text (which typically does not overlap with the texts in the CoNLL-U files).

In order to use the detokenizer, use the `detokenizer=file:filename_with_plaintext` option. In UD 1.2 models, the optimal performance is achieved with very small plain texts â€“ only 500kB.

In order to show the settings which were used by the UDPipe community when building the models made available when using `udpipe_download_model`, the tokenizer settings used for the different treebanks are shown below, so that you can easily use this to retrain your model directly on the corresponding UD treebank which you can download at http://universaldependencies.org/#ud-treebanks.


```r
data(udpipe_annotation_params)
str(udpipe_annotation_params$tokenizer)
```

```
'data.frame':	68 obs. of  9 variables:
 $ language_treebank   : chr  "ar" "be" "bg" "ca" ...
 $ dimension           : num  24 24 64 64 24 64 24 24 64 24 ...
 $ epochs              : num  100 100 100 100 100 100 100 100 100 100 ...
 $ initialization_range: num  0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ...
 $ batch_size          : num  100 100 100 50 50 50 50 50 50 50 ...
 $ learning_rate       : num  0.002 0.01 0.005 0.002 0.01 0.002 0.005 0.002 0.002 0.01 ...
 $ dropout             : num  0.3 0.2 0.2 0.1 0.2 0.1 0.1 0.1 0.3 0.2 ...
 $ early_stopping      : num  1 1 1 1 1 1 1 1 1 1 ...
 $ detokenize          : chr  NA NA NA NA ...
```

```r
## Example for training the tokenizer on the Dutch treebank
hyperparams_nl <- subset(udpipe_annotation_params$tokenizer, language_treebank == "nl")
as.list(hyperparams_nl)
```

```
$language_treebank
[1] "nl"

$dimension
[1] 24

$epochs
[1] 100

$initialization_range
[1] 0.1

$batch_size
[1] 100

$learning_rate
[1] 0.005

$dropout
[1] 0.1

$early_stopping
[1] 1

$detokenize
[1] NA
```

## Settings for the tagger:

The tagging is currently performed using MorphoDiTa (http://ufal.mff.cuni.cz/morphodita). The UDPipe tagger consists of possibly several MorphoDiTa models, each tagging some of the POS tags and/or lemmas.

By default, only one model is constructed, which generates all available tags (UPOS, XPOS, Feats and Lemma). However, we found out during the UD 1.2 models training that performance improves if one model tags the UPOS, XPOS and Feats tags, while the other is performing lemmatization. Therefore, if you utilize two MorphoDiTa models, by default the first one generates all tags (except lemmas) and the second one performs lemmatization.

The number of MorphoDiTa models can be specified using the models=number parameter. All other parameters may be either generic for all models (guesser_suffix_rules=5), or specific for a given model (guesser_suffix_rules_2=6), including the from_model option (therefore, MorphoDiTa models can be trained separately and then combined together into one UDPipe model).

Every model utilizes UPOS for disambiguation and the first model is the one producing the UPOS tags on output.

The tagger recognizes the following options:

- `use_lemma` (default for the second model and also if there is only one model): use the lemma field internally to perform disambiguation; the lemma may be not outputted
- `provide_lemma` (default for the second model and also if there is only one model): produce the disambiguated lemma on output
- `use_xpostag` (default for the first model): use the XPOS tags internally to perform disambiguation; it may not be outputted
- `provide_xpostag` (default for the first model): produce the disambiguated XPOS tag on output
- `use_feats` (default for the first model): use the Feats internally to perform disambiguation; it may not be outputted
- `provide_feats` (default for the first model): produce the disambiguated Feats field on output
- `dictionary_max_form_analyses` (default 0 - unlimited): the maximum number of (most frequent) form analyses from UD training data that are to be kept in the morphological dictionary
- `dictionary_file` (default empty): use a given custom morphological dictionary, where each line contains 5 tab-separated fields FORM, LEMMA, UPOSTAG, XPOSTAG and FEATS. Note that this dictionary data is appended to the dictionary created from the UD training data, not replacing it.
- `guesser_suffix_rules` (default 8): number of rules generated for every suffix
- `guesser_prefixes_max` (default 4 if provide_lemma, 0 otherwise): maximum number of form-generating prefixes to use in the guesser
- `guesser_prefix_min_count` (default 10): minimum number of occurrences of form-generating prefix to consider using it in the guesser
- `guesser_enrich_dictionary` (default 6 if no dictionary_file is passed, 0 otherwise): number of rules generated for forms present in training data (assuming that the analyses from the training data may not be all)
- `iterations` (default 20): number of training iterations to perform
- `early_stopping` (default 1 if heldout is given, 0 otherwise): perform early stopping, choosing training iteration maximizing tagging accuracy on the heldout data
- `templates` (default lemmatizer for second model, tagger otherwise): MorphoDiTa feature templates to use, either lemmatizer which focuses more on lemmas, or tagger which focuses more on UPOS/XPOS/FEATS

During random hyperparameter search, guesser_suffix_rules is chosen uniformly from {5,6,7,8,9,10,11,12} and guesser_enrich_dictionary is chosen uniformly from {3,4,5,6,7,8,9,10}. 

In order to show the settings which were used by the UDPipe community when building the models made available when using `udpipe_download_model`, the tagger settings used for the different treebanks are shown below, so that you can easily use this to retrain your model directly on the corresponding UD treebank which you can download at http://universaldependencies.org/#ud-treebanks.


```r
## Example for training the tagger on the Dutch treebank
hyperparams_nl <- subset(udpipe_annotation_params$tagger, language_treebank == "nl")
as.list(hyperparams_nl)
```

```
$language_treebank
[1] "nl"

$models
[1] 2

$templates_1
[1] "tagger"

$guesser_suffix_rules_1
[1] 8

$guesser_enrich_dictionary_1
[1] 6

$guesser_prefixes_max_1
[1] 0

$use_lemma_1
[1] 0

$use_xpostag_1
[1] 1

$use_feats_1
[1] 1

$provide_lemma_1
[1] 0

$provide_xpostag_1
[1] 1

$provide_feats_1
[1] 1

$prune_features_1
[1] 0

$templates_2
[1] "lemmatizer"

$guesser_suffix_rules_2
[1] 6

$guesser_enrich_dictionary_2
[1] 4

$guesser_prefixes_max_2
[1] 4

$use_lemma_2
[1] 1

$use_xpostag_2
[1] 0

$use_feats_2
[1] 0

$provide_lemma_2
[1] 1

$provide_xpostag_2
[1] 0

$provide_feats_2
[1] 0

$prune_features_2
[1] 0

$dictionary_max_form_analyses_2
[1] NA

$dictionary_max_form_analyses_1
[1] NA
```

## Settings for the dependency parser:

The parsing is performed using Parsito (http://ufal.mff.cuni.cz/parsito), which is a transition-based parser using a neural-network classifier.

The transition-based systems can be configured by the following options:

- `transition_system` (default projective): which transition system to use for parsing (language dependent, you can choose according to language properties or try all and choose the best one)
        `projective`: projective stack-based arc standard system with shift, left_arc and right_arc transitions
        `swap`: fully non-projective system which extends projective system by adding the swap transition
        `link2`: partially non-projective system which extends projective system by adding left_arc2 and right_arc2 transitions
- `transition_oracle` (default dynamic/static_lazy_static whichever first is applicable): which transition oracle to use for the chosen transition_system:
        `transition_system=projective`: available oracles are static and dynamic (dynamic usually gives better results, but training time is slower)
        `transition_system=swap`: available oracles are static_eager and static_lazy (static_lazy almost always gives better results)
        `transition_system=link2`: only available oracle is static
- `structured_interval` (default 8): use search-based oracle in addition to the translation_oracle specified. This almost always gives better results, but makes training 2-3 times slower. For details, see the paper Straka et al. 2015: Parsing Universal Dependency Treebanks using Neural Networks and Search-Based Oracle
- `single_root` (default 1): allow only single root when parsing, and make sure only the root node has the root deprel (note that training data are checked to be in this format)

The Lemmas/UPOS/XPOS/FEATS used by the parser are configured by:

- `use_gold_tags` (default 0): if false and a tagger exists, the Lemmas/UPOS/XPOS/FEATS for both the training and heldout data are generated by the tagger, otherwise they are taken from the gold data

The embeddings used by the parser can be specified as follows:

- `embedding_upostag` (default 20): the dimension of the UPos embedding used in the parser
- `embedding_feats` (default 20): the dimension of the Feats embedding used in the parser
- `embedding_xpostag` (default 0): the dimension of the XPos embedding used in the parser
- `embedding_form` (default 50): the dimension of the Form embedding used in the parser
- `embedding_lemma` (default 0): the dimension of the Lemma embedding used in the parser
- `embedding_deprel` (default 20): the dimension of the Deprel embedding used in the parser
- `embedding_form_file`: pre-trained word embeddings in word2vec textual format
- `embedding_lemma_file`: pre-trained lemma embeddings in word2vec textual format
- `embedding_form_mincount` (default 2): for forms not present in the pre-trained embeddings, generate random embeddings if the form appears at least this number of times in the trainig data (forms not present in the pre-trained embeddings and appearing less number of times are considered OOV)
- `embedding_lemma_mincount` (default 2): for lemmas not present in the pre-trained embeddings, generate random embeddings if the lemma appears at least this number of times in the trainig data (lemmas not present in the pre-trained embeddings and appearing less number of times are considered OOV)

The neural-network training options:

- `iterations` (default 10): number of training iterations to use
- `hidden_layer` (default 200): the size of the hidden layer
- `batch_size` (default 10): batch size used during neural-network training
- `learning_rate` (default 0.02): the learning rate used during neural-network training
- `learning_rate_final` (0.001): the final learning rate used during neural-network training
- `l2` (0.5): the L2 regularization used during neural-network training
- `early_stopping` (default 1 if heldout is given, 0 otherwise): perform early stopping, choosing training iteration maximizing LAS on heldout data

During random hyperparameter search, structured_interval is chosen uniformly from {0,8,10}, learning_rate is chosen logarithmically from <0.005,0.04) and l2 is chosen uniformly from <0.2,0.6). 

In order to show the settings which were used by the UDPipe community when building the models made available when using `udpipe_download_model`, the parser settings used for the different treebanks are shown below, so that you can easily use this to retrain your model directly on the corresponding UD treebank which you can download at http://universaldependencies.org/#ud-treebanks.


```r
## Example for training the dependency parser on the Dutch treebank
hyperparams_nl <- subset(udpipe_annotation_params$parser, language_treebank == "nl")
as.list(hyperparams_nl)
```

```
$language_treebank
[1] "nl"

$iterations
[1] 30

$embedding_upostag
[1] 20

$embedding_feats
[1] 20

$embedding_xpostag
[1] 0

$embedding_form
[1] 50

$embedding_form_file
[1] "../ud-2.0-embeddings/nl.skip.forms.50.vectors"

$embedding_lemma
[1] 0

$embedding_deprel
[1] 20

$learning_rate
[1] 0.01

$learning_rate_final
[1] 0.001

$l2
[1] 0.5

$hidden_layer
[1] 200

$batch_size
[1] 10

$transition_system
[1] "projective"

$transition_oracle
[1] "dynamic"

$structured_interval
[1] 10
```

## Example

### Example on UD 2.6 on German GSD 

The following code shows a complete training run on the German GSD treebank from UD 2.6. The code 

1. downloads the Universal Dependencies training data
2. builds a word2vec model on top of the training data as this is used in the dependency parser
3. trains the model using the set of hyperparameters which were used by the UDPipe authors and which you can find at https://github.com/ufal/udpipe/tree/master/training in general or at https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-3131 for UD 2.5

```r
library(utils)
library(udpipe)
library(word2vec)

## Work on data from Universal Dependencies - German GSD treebank
settings <- list()
settings$ud.train    <- "https://raw.githubusercontent.com/UniversalDependencies/UD_German-GSD/r2.6/de_gsd-ud-train.conllu"
settings$ud.dev      <- "https://raw.githubusercontent.com/UniversalDependencies/UD_German-GSD/r2.6/de_gsd-ud-dev.conllu"
settings$ud.test     <- "https://raw.githubusercontent.com/UniversalDependencies/UD_German-GSD/r2.6/de_gsd-ud-test.conllu"

## Download the conllu files
download.file(url = settings$ud.train, destfile = "train.conllu")
download.file(url = settings$ud.dev,   destfile = "dev.conllu")
download.file(url = settings$ud.test,  destfile = "test.conllu")

## Create wordvectors as these are used for training the dependency parser + save the word vectors to disk
x <- udpipe_read_conllu("train.conllu")
x <- paste.data.frame(x, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"), collapse = " ")
x <- x$token
writeLines(x, con = file("text.txt", encoding = "UTF-8", open = "wt"))
w2v <- word2vec("text.txt", type = "skip-gram", dim = 50, window = 10, min_count = 2, negative = 5, iter = 15, threads = 1)
write.word2vec(w2v, file = "wordvectors.vec", type = "txt", encoding = "UTF-8")
predict(w2v, c("gut", "freundlich"), type = "nearest", top = 20)

## Train the model
print(Sys.time())
m <- udpipe_train(file = "de_gsd-ud-2.6-20200924.udpipe", 
                  files_conllu_training = "train.conllu", 
                  files_conllu_holdout  = "dev.conllu",
                  annotation_tokenizer = list(dimension = 64, epochs = 100, segment_size=200, initialization_range = 0.1, 
                                              batch_size = 50, learning_rate = 0.002, learning_rate_final=0, dropout = 0.1, early_stopping = 1),
                  annotation_tagger = list(models = 2, 
                                           templates_1 = "lemmatizer", guesser_suffix_rules_1 = 8, guesser_enrich_dictionary_1 = 4, guesser_prefixes_max_1 = 4, 
                                           use_lemma_1 = 1,provide_lemma_1 = 1, use_xpostag_1 = 0, provide_xpostag_1 = 0, 
                                           use_feats_1 = 0, provide_feats_1 = 0, prune_features_1 = 1, 
                                           templates_2 = "tagger", guesser_suffix_rules_2 = 8, guesser_enrich_dictionary_2 = 4, guesser_prefixes_max_2 = 0, 
                                           use_lemma_2 = 1, provide_lemma_2 = 0, use_xpostag_2 = 1, provide_xpostag_2 = 1, 
                                           use_feats_2 = 1, provide_feats_2 = 1, prune_features_2 = 1),
                  annotation_parser = list(iterations = 30, embedding_upostag = 20, embedding_feats = 20, embedding_xpostag = 0, 
                                           embedding_form = 50, embedding_form_file = "wordvectors.vec", 
                                           embedding_lemma = 0, embedding_deprel = 20, learning_rate = 0.01, 
                                           learning_rate_final = 0.001, l2 = 0.5, hidden_layer = 200, 
                                           batch_size = 10, transition_system = "projective", transition_oracle = "dynamic", 
                                           structured_interval = 8))
print(Sys.time())

## Evaluate the accuracy
m <- udpipe_load_model("de_gsd-ud-2.6-20200924.udpipe")
goodness_of_fit <- udpipe_accuracy(m, "test.conllu", tokenizer = "default", tagger = "default", parser = "default")
cat(goodness_of_fit$accuracy, sep = "\n") 
goodness_of_fit <- udpipe_accuracy(m, "test.conllu", tokenizer = "none", tagger = "default", parser = "default")
cat(goodness_of_fit$accuracy, sep = "\n") 
goodness_of_fit <- udpipe_accuracy(m, "test.conllu", tokenizer = "none", tagger = "none", parser = "default")
cat(goodness_of_fit$accuracy, sep = "\n") 
```

This will give you something similar to this:

```
> print(Sys.time())
[1] "2020-09-24 22:42:30 CEST"
> m <- udpipe_train(file = "de_gsd-ud-2.6-20200924.udpipe", 
+                   files_conllu_training = "train.conllu", 
+                   files_conllu_holdout  = "dev.conllu",
+                   annotation_tokenizer = list(dimension = 64, epochs = 100, segment_size=200, initialization_range = 0.1, 
+                                               batch_size = 50, learning_rate = 0.002, learning_rate_final=0, dropout = 0.1, early_stopping = 1),
+                   annotation_tagger = list(models = 2, 
+                                            templates_1 = "lemmatizer", guesser_suffix_rules_1 = 8, guesser_enrich_dictionary_1 = 4, guesser_prefixes_max_1 = 4, 
+                                            use_lemma_1 = 1,provide_lemma_1 = 1, use_xpostag_1 = 0, provide_xpostag_1 = 0, 
+                                            use_feats_1 = 0, provide_feats_1 = 0, prune_features_1 = 1, 
+                                            templates_2 = "tagger", guesser_suffix_rules_2 = 8, guesser_enrich_dictionary_2 = 4, guesser_prefixes_max_2 = 0, 
+                                            use_lemma_2 = 1, provide_lemma_2 = 0, use_xpostag_2 = 1, provide_xpostag_2 = 1, 
+                                            use_feats_2 = 1, provide_feats_2 = 1, prune_features_2 = 1),
+                   annotation_parser = list(iterations = 30, embedding_upostag = 20, embedding_feats = 20, embedding_xpostag = 0, 
+                                            embedding_form = 50, embedding_form_file = "wordvectors.vec", 
+                                            embedding_lemma = 0, embedding_deprel = 20, learning_rate = 0.01, 
+                                            learning_rate_final = 0.001, l2 = 0.5, hidden_layer = 200, 
+                                            batch_size = 10, transition_system = "projective", transition_oracle = "dynamic", 
+                                            structured_interval = 8))
Training tokenizer with the following options: tokenize_url=1, allow_spaces=0, dimension=64
  epochs=100, batch_size=50, learning_rate=0.0020, dropout=0.1000, early_stopping=1
Epoch 1, logprob: -6.9193e+004, training acc: 95.43%, heldout tokens: 97.44%P/96.73%R/97.08%, sentences: 79.03%P/79.22%R/79.12%
Epoch 2, logprob: -5.3557e+003, training acc: 99.58%, heldout tokens: 98.33%P/98.73%R/98.53%, sentences: 82.25%P/84.11%R/83.17%
Epoch 3, logprob: -4.7095e+003, training acc: 99.62%, heldout tokens: 98.84%P/99.25%R/99.04%, sentences: 82.72%P/87.48%R/85.04%
Epoch 4, logprob: -4.2784e+003, training acc: 99.65%, heldout tokens: 98.99%P/99.33%R/99.16%, sentences: 86.53%P/89.24%R/87.86%
Epoch 5, logprob: -3.9963e+003, training acc: 99.67%, heldout tokens: 99.37%P/99.43%R/99.40%, sentences: 88.16%P/89.49%R/88.82%
Epoch 6, logprob: -3.6930e+003, training acc: 99.70%, heldout tokens: 99.47%P/99.29%R/99.38%, sentences: 91.18%P/90.61%R/90.90%
Epoch 7, logprob: -3.6024e+003, training acc: 99.72%, heldout tokens: 99.56%P/99.59%R/99.58%, sentences: 90.19%P/90.86%R/90.52%
Epoch 8, logprob: -3.3887e+003, training acc: 99.74%, heldout tokens: 99.26%P/99.46%R/99.36%, sentences: 89.80%P/91.49%R/90.64%
Epoch 9, logprob: -3.3730e+003, training acc: 99.74%, heldout tokens: 99.61%P/99.72%R/99.66%, sentences: 91.81%P/91.24%R/91.53%
Epoch 10, logprob: -3.2727e+003, training acc: 99.74%, heldout tokens: 99.52%P/99.59%R/99.55%, sentences: 89.15%P/91.49%R/90.30%
Epoch 11, logprob: -3.2605e+003, training acc: 99.74%, heldout tokens: 99.68%P/99.69%R/99.68%, sentences: 91.60%P/91.49%R/91.55%
Epoch 12, logprob: -3.1610e+003, training acc: 99.75%, heldout tokens: 99.66%P/99.77%R/99.72%, sentences: 89.52%P/90.86%R/90.19%
Epoch 13, logprob: -3.1630e+003, training acc: 99.75%, heldout tokens: 99.58%P/99.55%R/99.56%, sentences: 92.69%P/91.99%R/92.34%
Epoch 14, logprob: -3.0341e+003, training acc: 99.76%, heldout tokens: 99.59%P/99.60%R/99.60%, sentences: 91.59%P/91.36%R/91.48%
Epoch 15, logprob: -2.9909e+003, training acc: 99.77%, heldout tokens: 99.57%P/99.71%R/99.64%, sentences: 91.79%P/92.37%R/92.08%
Epoch 16, logprob: -2.9968e+003, training acc: 99.77%, heldout tokens: 99.70%P/99.76%R/99.73%, sentences: 91.30%P/91.99%R/91.65%
Epoch 17, logprob: -2.8742e+003, training acc: 99.78%, heldout tokens: 99.62%P/99.76%R/99.69%, sentences: 91.65%P/91.99%R/91.82%
Epoch 18, logprob: -2.8592e+003, training acc: 99.77%, heldout tokens: 99.71%P/99.69%R/99.70%, sentences: 92.10%P/91.86%R/91.98%
Epoch 19, logprob: -2.8845e+003, training acc: 99.77%, heldout tokens: 99.62%P/99.68%R/99.65%, sentences: 92.42%P/93.12%R/92.77%
Epoch 20, logprob: -2.8405e+003, training acc: 99.77%, heldout tokens: 99.68%P/99.71%R/99.69%, sentences: 93.20%P/92.62%R/92.91%
Epoch 21, logprob: -2.7477e+003, training acc: 99.78%, heldout tokens: 99.69%P/99.78%R/99.74%, sentences: 89.19%P/90.86%R/90.02%
Epoch 22, logprob: -2.7414e+003, training acc: 99.78%, heldout tokens: 99.59%P/99.65%R/99.62%, sentences: 90.73%P/91.86%R/91.29%
Epoch 23, logprob: -2.6864e+003, training acc: 99.79%, heldout tokens: 99.66%P/99.59%R/99.63%, sentences: 92.80%P/93.62%R/93.21%
Epoch 24, logprob: -2.4937e+003, training acc: 99.80%, heldout tokens: 99.72%P/99.76%R/99.74%, sentences: 92.67%P/93.37%R/93.02%
Epoch 25, logprob: -2.6481e+003, training acc: 99.79%, heldout tokens: 99.73%P/99.78%R/99.76%, sentences: 91.96%P/92.99%R/92.47%
Epoch 26, logprob: -2.6341e+003, training acc: 99.80%, heldout tokens: 99.66%P/99.75%R/99.70%, sentences: 93.12%P/93.24%R/93.18%
Epoch 27, logprob: -2.6092e+003, training acc: 99.79%, heldout tokens: 99.72%P/99.72%R/99.72%, sentences: 93.37%P/93.37%R/93.37%
Epoch 28, logprob: -2.5791e+003, training acc: 99.79%, heldout tokens: 99.66%P/99.59%R/99.63%, sentences: 94.51%P/92.62%R/93.55%
Epoch 29, logprob: -2.4441e+003, training acc: 99.81%, heldout tokens: 99.69%P/99.69%R/99.69%, sentences: 94.45%P/93.74%R/94.10%
Epoch 30, logprob: -2.6083e+003, training acc: 99.80%, heldout tokens: 99.74%P/99.77%R/99.76%, sentences: 94.01%P/94.24%R/94.12%
Epoch 31, logprob: -2.5544e+003, training acc: 99.80%, heldout tokens: 99.47%P/99.26%R/99.37%, sentences: 92.49%P/92.49%R/92.49%
Epoch 32, logprob: -2.4888e+003, training acc: 99.79%, heldout tokens: 99.76%P/99.79%R/99.78%, sentences: 94.20%P/93.49%R/93.84%
Epoch 33, logprob: -2.5314e+003, training acc: 99.80%, heldout tokens: 99.72%P/99.73%R/99.72%, sentences: 93.25%P/93.37%R/93.31%
Epoch 34, logprob: -2.4597e+003, training acc: 99.81%, heldout tokens: 99.74%P/99.80%R/99.77%, sentences: 93.23%P/93.12%R/93.17%
Epoch 35, logprob: -2.3949e+003, training acc: 99.81%, heldout tokens: 99.68%P/99.76%R/99.72%, sentences: 91.81%P/92.62%R/92.21%
Epoch 36, logprob: -2.3642e+003, training acc: 99.81%, heldout tokens: 99.71%P/99.76%R/99.74%, sentences: 94.09%P/93.62%R/93.85%
Epoch 37, logprob: -2.4537e+003, training acc: 99.81%, heldout tokens: 99.73%P/99.80%R/99.76%, sentences: 92.04%P/92.62%R/92.33%
Epoch 38, logprob: -2.4156e+003, training acc: 99.80%, heldout tokens: 99.74%P/99.79%R/99.76%, sentences: 93.75%P/91.99%R/92.86%
Epoch 39, logprob: -2.4693e+003, training acc: 99.81%, heldout tokens: 99.59%P/99.42%R/99.51%, sentences: 93.23%P/93.12%R/93.17%
Epoch 40, logprob: -2.3308e+003, training acc: 99.82%, heldout tokens: 99.67%P/99.63%R/99.65%, sentences: 93.14%P/93.49%R/93.32%
Epoch 41, logprob: -2.3365e+003, training acc: 99.81%, heldout tokens: 99.70%P/99.73%R/99.72%, sentences: 92.73%P/92.62%R/92.67%
Epoch 42, logprob: -2.3616e+003, training acc: 99.81%, heldout tokens: 99.72%P/99.75%R/99.73%, sentences: 93.14%P/93.49%R/93.32%
Epoch 43, logprob: -2.3260e+003, training acc: 99.81%, heldout tokens: 99.78%P/99.83%R/99.81%, sentences: 93.49%P/93.49%R/93.49%
Epoch 44, logprob: -2.3651e+003, training acc: 99.81%, heldout tokens: 99.73%P/99.76%R/99.74%, sentences: 92.14%P/92.49%R/92.32%
Epoch 45, logprob: -2.2028e+003, training acc: 99.82%, heldout tokens: 99.69%P/99.70%R/99.70%, sentences: 94.21%P/93.62%R/93.91%
Epoch 46, logprob: -2.2973e+003, training acc: 99.82%, heldout tokens: 99.62%P/99.60%R/99.61%, sentences: 93.62%P/93.74%R/93.68%
Epoch 47, logprob: -2.3190e+003, training acc: 99.81%, heldout tokens: 99.77%P/99.79%R/99.78%, sentences: 93.52%P/93.87%R/93.69%
Epoch 48, logprob: -2.2889e+003, training acc: 99.81%, heldout tokens: 99.74%P/99.75%R/99.74%, sentences: 93.34%P/92.99%R/93.17%
Epoch 49, logprob: -2.2343e+003, training acc: 99.82%, heldout tokens: 99.75%P/99.76%R/99.76%, sentences: 93.63%P/93.87%R/93.75%
Epoch 50, logprob: -2.1653e+003, training acc: 99.82%, heldout tokens: 99.72%P/99.73%R/99.72%, sentences: 93.85%P/93.62%R/93.73%
Epoch 51, logprob: -2.1677e+003, training acc: 99.83%, heldout tokens: 99.76%P/99.82%R/99.79%, sentences: 93.91%P/92.62%R/93.26%
Epoch 52, logprob: -2.2911e+003, training acc: 99.83%, heldout tokens: 99.76%P/99.77%R/99.76%, sentences: 94.02%P/92.49%R/93.25%
Epoch 53, logprob: -2.2195e+003, training acc: 99.82%, heldout tokens: 99.74%P/99.77%R/99.76%, sentences: 94.10%P/93.74%R/93.92%
Epoch 54, logprob: -2.1996e+003, training acc: 99.83%, heldout tokens: 99.72%P/99.78%R/99.75%, sentences: 92.99%P/92.99%R/92.99%
Epoch 55, logprob: -2.1129e+003, training acc: 99.83%, heldout tokens: 99.72%P/99.76%R/99.74%, sentences: 93.12%P/93.12%R/93.12%
Epoch 56, logprob: -2.2491e+003, training acc: 99.81%, heldout tokens: 99.68%P/99.72%R/99.70%, sentences: 93.61%P/93.49%R/93.55%
Epoch 57, logprob: -2.1532e+003, training acc: 99.83%, heldout tokens: 99.77%P/99.76%R/99.76%, sentences: 94.31%P/93.37%R/93.84%
Epoch 58, logprob: -2.0931e+003, training acc: 99.83%, heldout tokens: 99.75%P/99.81%R/99.78%, sentences: 93.57%P/92.87%R/93.22%
Epoch 59, logprob: -2.1038e+003, training acc: 99.83%, heldout tokens: 99.70%P/99.66%R/99.68%, sentences: 93.23%P/93.12%R/93.17%
Epoch 60, logprob: -2.2800e+003, training acc: 99.82%, heldout tokens: 99.76%P/99.82%R/99.79%, sentences: 93.82%P/93.12%R/93.47%
Epoch 61, logprob: -2.1557e+003, training acc: 99.82%, heldout tokens: 99.72%P/99.75%R/99.73%, sentences: 93.44%P/92.74%R/93.09%
Stopping after 30 iterations of not improving sum of sentence and token f1.
Choosing parameters from epoch 30.
Tagger model 1 columns: lemma use=1/provide=1, xpostag use=0/provide=0, feats use=0/provide=0
Creating morphological dictionary for tagger model 1.
Tagger model 1 dictionary options: max_form_analyses=0, custom dictionary_file=none
Tagger model 1 guesser options: suffix_rules=8, prefixes_max=4, prefix_min_count=10, enrich_dictionary=4
Tagger model 1 options: iterations=20, early_stopping=1, templates=lemmatizer
Training tagger model 1.
Iteration 1: done, accuracy 85.97%, heldout accuracy 90.79%t/95.47%l/87.59%b
Iteration 2: done, accuracy 93.36%, heldout accuracy 91.26%t/96.16%l/88.64%b
Iteration 3: done, accuracy 94.85%, heldout accuracy 91.30%t/96.23%l/88.72%b
Iteration 4: done, accuracy 95.78%, heldout accuracy 91.39%t/96.24%l/88.82%b
Iteration 5: done, accuracy 96.46%, heldout accuracy 91.53%t/96.30%l/89.02%b
Iteration 6: done, accuracy 96.78%, heldout accuracy 91.66%t/96.28%l/89.09%b
Iteration 7: done, accuracy 97.27%, heldout accuracy 91.73%t/96.27%l/89.11%b
Iteration 8: done, accuracy 97.55%, heldout accuracy 91.76%t/96.24%l/89.12%b
Iteration 9: done, accuracy 97.89%, heldout accuracy 91.81%t/96.30%l/89.18%b
Iteration 10: done, accuracy 98.05%, heldout accuracy 91.75%t/96.24%l/89.10%b
Iteration 11: done, accuracy 98.29%, heldout accuracy 91.73%t/96.20%l/89.06%b
Iteration 12: done, accuracy 98.41%, heldout accuracy 91.72%t/96.14%l/89.01%b
Iteration 13: done, accuracy 98.46%, heldout accuracy 91.73%t/96.10%l/88.98%b
Iteration 14: done, accuracy 98.67%, heldout accuracy 91.73%t/96.12%l/88.96%b
Iteration 15: done, accuracy 98.71%, heldout accuracy 91.74%t/96.11%l/88.97%b
Iteration 16: done, accuracy 98.84%, heldout accuracy 91.74%t/96.12%l/88.99%b
Iteration 17: done, accuracy 98.85%, heldout accuracy 91.74%t/96.09%l/88.96%b
Iteration 18: done, accuracy 98.90%, heldout accuracy 91.76%t/96.04%l/88.96%b
Iteration 19: done, accuracy 99.05%, heldout accuracy 91.73%t/96.03%l/88.92%b
Iteration 20: done, accuracy 99.14%, heldout accuracy 91.70%t/95.99%l/88.84%b
Chosen tagger model from iteration 9
Tagger model 2 columns: lemma use=1/provide=0, xpostag use=1/provide=1, feats use=1/provide=1
Creating morphological dictionary for tagger model 2.
Tagger model 2 dictionary options: max_form_analyses=0, custom dictionary_file=none
Tagger model 2 guesser options: suffix_rules=8, prefixes_max=0, prefix_min_count=10, enrich_dictionary=4
Tagger model 2 options: iterations=20, early_stopping=1, templates=tagger
Training tagger model 2.
Iteration 1: done, accuracy 80.17%, heldout accuracy 60.42%t/92.10%l/58.61%b
Iteration 2: done, accuracy 89.15%, heldout accuracy 61.48%t/92.47%l/59.71%b
Iteration 3: done, accuracy 92.28%, heldout accuracy 61.88%t/92.54%l/60.13%b
Iteration 4: done, accuracy 94.10%, heldout accuracy 61.90%t/92.54%l/60.19%b
Iteration 5: done, accuracy 95.40%, heldout accuracy 62.22%t/92.54%l/60.53%b
Iteration 6: done, accuracy 96.25%, heldout accuracy 62.24%t/92.47%l/60.56%b
Iteration 7: done, accuracy 96.95%, heldout accuracy 62.33%t/92.47%l/60.63%b
Iteration 8: done, accuracy 97.47%, heldout accuracy 62.45%t/92.53%l/60.76%b
Iteration 9: done, accuracy 97.76%, heldout accuracy 62.39%t/92.48%l/60.70%b
Iteration 10: done, accuracy 98.19%, heldout accuracy 62.57%t/92.50%l/60.88%b
Iteration 11: done, accuracy 98.34%, heldout accuracy 62.58%t/92.50%l/60.90%b
Iteration 12: done, accuracy 98.56%, heldout accuracy 62.59%t/92.47%l/60.91%b
Iteration 13: done, accuracy 98.71%, heldout accuracy 62.66%t/92.45%l/60.95%b
Iteration 14: done, accuracy 98.87%, heldout accuracy 62.68%t/92.43%l/60.96%b
Iteration 15: done, accuracy 98.98%, heldout accuracy 62.73%t/92.46%l/61.02%b
Iteration 16: done, accuracy 99.12%, heldout accuracy 62.75%t/92.46%l/61.05%b
Iteration 17: done, accuracy 99.18%, heldout accuracy 62.75%t/92.42%l/61.04%b
Iteration 18: done, accuracy 99.27%, heldout accuracy 62.89%t/92.46%l/61.16%b
Iteration 19: done, accuracy 99.30%, heldout accuracy 62.85%t/92.45%l/61.15%b
Iteration 20: done, accuracy 99.33%, heldout accuracy 62.86%t/92.47%l/61.16%b
Chosen tagger model from iteration 18
Parser transition options: system=projective, oracle=dynamic, structured_interval=8, single_root=1
Parser uses lemmas/upos/xpos/feats: automatically generated by tagger
Parser embeddings options: upostag=20, feats=20, xpostag=0, form=50, lemma=0, deprel=20
  form mincount=2, precomputed form embeddings=wordvectors.vec
  lemma mincount=2, precomputed lemma embeddings=none
Parser network options: iterations=30, hidden_layer=200, batch_size=10,
  learning_rate=0.0100, learning_rate_final=0.0010, l2=0.5000, early_stopping=1
Initialized 'universal_tag' embedding with 0,16 words and 0.0%,100.0% coverage.
Initialized 'feats' embedding with 0,328 words and 0.0%,100.0% coverage.
Initialized 'form' embedding with 15101,15303 words and 74.1%,87.6% coverage.
Initialized 'deprel' embedding with 0,42 words and 0.0%,100.0% coverage.
Iteration 1: training logprob -2.6984e+005, heldout UAS 73.54%, LAS 65.82%
Iteration 2: training logprob -4.5160e+005, heldout UAS 68.76%, LAS 61.06%
Iteration 3: training logprob -4.0263e+005, heldout UAS 72.26%, LAS 64.58%
Iteration 4: training logprob -3.7276e+005, heldout UAS 72.59%, LAS 65.67%
Iteration 5: training logprob -3.5034e+005, heldout UAS 73.51%, LAS 66.70%
Iteration 6: training logprob -3.3267e+005, heldout UAS 73.25%, LAS 65.88%
Iteration 7: training logprob -3.1554e+005, heldout UAS 75.35%, LAS 68.21%
Iteration 8: training logprob -2.9980e+005, heldout UAS 76.47%, LAS 69.86%
Iteration 9: training logprob training logprob -2.8442e+005, heldout UAS 75.99%, LAS 69.81%
Iteration 10: training logprob training logprob -2.7191e+005, heldout UAS 77.95%, LAS 71.58%
Iteration 11: training logprob -2.5799e+005, heldout UAS 77.02%, LAS 70.56%
Iteration 12: training logprob training logprob -2.5025e+005, heldout UAS 77.68%, LAS 71.32%
Iteration 13: training logprob -2.3856e+005, heldout UAS 77.94%, LAS 72.20%
Iteration 14: training logprob -2.3122e+005training logprob -2.5025e+005, heldout UAS 77.68%, LAS 71.32%
Iteration 13: training logprob -2.3856e+005, heldout UAS 77.94%, LAS 72.20%
Iteration 14: training logprob -2.3122e+005, heldout UAS 77.60%, LAS 71.27%
Iteration 15: training logprob training logprob -2.5025e+005, heldout UAS 77.68%, LAS 71.32%
Iteration 13: training logprob -2.3856e+005, heldout UAS 77.94%, LAS 72.20%
Iteration 14: training logprob -2.3122e+005, heldout UAS 77.60%, LAS 71.27%
Iteration 15: training logprob -2.2353e+005, heldout UAS 77.40%, LAS 71.45%
Iteration 16: training logprob -2.1468e+005, heldout UAS 79.22%, LAS 72.95%
Iteration 17: training logprob -2.0689e+005, heldout UAS 79.31%, LAS 73.19%
Iteration 18: training logprob -2.0281e+005, heldout UAS 78.96%, LAS 72.91%
Iteration 19: training logprob -1.9567e+005, heldout UAS 79.10%, LAS 73.27%
Iteration 20: training logprob -1.9331e+005, heldout UAS 79.86%, LAS 74.23%
Iteration 21: training logprob -1.8678e+005, heldout UAS 80.10%, LAS 74.18%
Iteration 22: training logprob -1.8485e+005, heldout UAS 79.65%, LAS 73.87%
Iteration 23: training logprob -1.8144e+005, heldout UAS 79.15%, LAS 73.04%
Iteration 24: training logprob -1.7638e+005, heldout UAS 79.22%, LAS 73.43%
Iteration 25: training logprob -1.7619e+005, heldout UAS 79.86%, LAS 73.97%
Iteration 26: training logprob -1.7434e+005, heldout UAS 79.97%, LAS 74.50%
Iteration 27: training logprob -1.6983e+005, heldout UAS 80.18%, LAS 74.40%
Iteration 28: training logprob -1.6875e+005, heldout UAS 80.53%, LAS 74.58%
Iteration 29: training logprob -1.6827e+005, heldout UAS 79.67%, LAS 73.95%
Iteration 30: training logprob -1.6893e+005, heldout UAS 80.22%, LAS 74.60%
Using early stopping -- choosing network from iteration 30
> print(Sys.time())
[1] "2020-09-25 21:40:46 CEST"

> ## Evaluate the accuracy
> m <- udpipe_load_model("de_gsd-ud-2.6-20200924.udpipe")
> goodness_of_fit <- udpipe_accuracy(m, "test.conllu", tokenizer = "default", tagger = "default", parser = "default")
> cat(goodness_of_fit$accuracy, sep = "\n") 
Number of SpaceAfter=No features in gold data: 2423
Tokenizer tokens - system: 16230, gold: 16224, precision: 99.45%, recall: 99.49%, f1: 99.47%
Tokenizer multiword tokens - system: 274, gold: 274, precision: 100.00%, recall: 100.00%, f1: 100.00%
Tokenizer words - system: 16504, gold: 16498, precision: 99.46%, recall: 99.50%, f1: 99.48%
Tokenizer sentences - system: 913, gold: 977, precision: 83.24%, recall: 77.79%, f1: 80.42%
Tagging from plain text (CoNLL17 F1 score) - gold forms: 16498, upostag: 91.55%, xpostag: 79.48%, feats: 69.74%, alltags: 62.84%, lemmas: 95.29%
Parsing from plain text with computed tags (CoNLL17 F1 score) - gold forms: 16498, UAS: 77.10%, LAS: 71.67%
> goodness_of_fit <- udpipe_accuracy(m, "test.conllu", tokenizer = "none", tagger = "default", parser = "default")
> cat(goodness_of_fit$accuracy, sep = "\n") 
Tagging from gold tokenization - forms: 16498, upostag: 92.14%, xpostag: 79.80%, feats: 70.18%, alltags: 63.35%, lemmas: 95.78%
Parsing from gold tokenization with computed tags - forms: 16498, UAS: 79.88%, LAS: 74.22%
> goodness_of_fit <- udpipe_accuracy(m, "test.conllu", tokenizer = "none", tagger = "none", parser = "default")
> cat(goodness_of_fit$accuracy, sep = "\n") 
Parsing from gold tokenization with gold tags - forms: 16498, UAS: 84.24%, LAS: 79.77%
```

Which gives accuracy statistics similar to the officially released UDPipe models for this treebank.
Note that model training takes a while depending on the size of the treebank and your hyperparameter settings. This example was run on a Windows i5 CPU laptop with 1.7Ghz, so no GPU needed. 
Good luck!

## Support in text mining

Need support in text mining. 
Contact BNOSAC: http://www.bnosac.be




