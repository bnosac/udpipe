#' @title Download an UDPipe model provided by the UDPipe community for a specific language of choice
#' @description 
#' Ready-made models for 61 languages trained on 90 treebanks from \url{http://universaldependencies.org/} are provided to you.
#' Some of these models were provided by the UDPipe community. Other models were build using this R package.
#' You can either download these models manually in order to use it for annotation purposes 
#' or use \code{udpipe_download_model} to download these models for a specific language of choice. You have the following options: \cr
#' @param language a character string with a Universal Dependencies treebank which was used to build the model. Possible values are:\cr
#' afrikaans-afribooms, ancient_greek-perseus, ancient_greek-proiel, arabic-padt, armenian-armtdp, basque-bdt, belarusian-hse, 
#' bulgarian-btb, buryat-bdt, catalan-ancora, chinese-gsd, coptic-scriptorium, croatian-set, czech-cac, czech-cltt, 
#' czech-fictree, czech-pdt, danish-ddt, dutch-alpino, dutch-lassysmall, english-ewt, english-gum, english-lines, 
#' english-partut, estonian-edt, finnish-ftb, finnish-tdt, french-gsd, french-partut, french-sequoia, french-spoken, 
#' galician-ctg, galician-treegal, german-gsd, gothic-proiel, greek-gdt, hebrew-htb, hindi-hdtb, hungarian-szeged, 
#' indonesian-gsd, irish-idt, italian-isdt, italian-partut, italian-postwita, japanese-gsd, kazakh-ktb, korean-gsd, 
#' korean-kaist, kurmanji-mg, latin-ittb, latin-perseus, latin-proiel, latvian-lvtb, lithuanian-hse, maltese-mudt, 
#' marathi-ufal, north_sami-giella, norwegian-bokmaal, norwegian-nynorsk, norwegian-nynorsklia, old_church_slavonic-proiel, 
#' old_french-srcmf, persian-seraji, polish-lfg, polish-sz, portuguese-bosque, portuguese-br, portuguese-gsd, 
#' romanian-nonstandard, romanian-rrt, russian-gsd, russian-syntagrus, russian-taiga, sanskrit-ufal, serbian-set, 
#' slovak-snk, slovenian-ssj, slovenian-sst, spanish-ancora, spanish-gsd, swedish-lines, swedish-talbanken, tamil-ttb, 
#' telugu-mtg, turkish-imst, ukrainian-iu, upper_sorbian-ufal, urdu-udtb, uyghur-udt, vietnamese-vtb \cr 
#' 
#' Each language should have a treebank extenstion (e.g. english-ewt, russian-syntagrus, dutch-alpino, ...). 
#' If you do not provide a treebank extension (e.g. only english, russian, dutch), 
#' the function will use the default treebank of that language as was used in Universal Dependencies up to version 2.1.
#' @param model_dir a path where the model will be downloaded to. Defaults to the current working directory
#' @param udpipe_model_repo location where the models will be downloaded from. 
#' Either 'jwijffels/udpipe.models.ud.2.3', 'jwijffels/udpipe.models.ud.2.0', 'jwijffels/udpipe.models.conll18.baseline' or 'bnosac/udpipe.models.ud'. \cr
#' Defaults to 'jwijffels/udpipe.models.ud.2.3'. \cr
#' \itemize{
#'   \item{'jwijffels/udpipe.models.ud.2.3' contains models released under the CC-BY-NC-SA license constructed on Universal Dependencies 2.3 data}
#'   \item{'jwijffels/udpipe.models.ud.2.0' contains models released under the CC-BY-NC-SA license constructed on Universal Dependencies 2.0 data}
#'   \item{'jwijffels/udpipe.models.conll18.baseline' contains models released under the CC-BY-NC-SA license constructed on Universal Dependencies 2.2 data for the 2018 conll shared task}
#'   \item{'bnosac/udpipe.models.ud' contains models mainly released under the CC-BY-SA license constructed on Universal Dependencies 2.1 data, and some models released under the GPL-3 and LGPL-LR license}
#' }
#' See the Details section for further information on which languages are available in each of these repositories.
#' @param overwrite logical indicating to overwrite the file if the file was already downloaded. Defaults to \code{TRUE} indicating 
#' it will download the model and overwrite the file if the file already existed. If set to \code{FALSE},
#' the model will only be downloaded if it does not exist on disk yet in the \code{model_dir} folder.
#' @param ... currently not used
#' @return A data.frame with 1 row and 3 columns: 
#' \itemize{
#'  \item{language: }{The language as provided by the input parameter \code{language}}
#'  \item{file_model: }{The path to the file on disk where the model was downloaded to}
#'  \item{url: }{The URL where the model was downloaded from}
#' }
#' @seealso \code{\link{udpipe_load_model}}
#' @details 
#' The function allows you to download the following language models based on your setting of argument \code{udpipe_model_repo}:
#' \itemize{
#'   \item 'jwijffels/udpipe.models.ud.2.3': \url{https://github.com/jwijffels/udpipe.models.ud.2.3}
#'     \itemize{
#'       \item{UDPipe models constructed on data from Universal Dependencies 2.3}
#'       \item{languages-treebanks: afrikaans-afribooms, ancient_greek-perseus, ancient_greek-proiel, arabic-padt, armenian-armtdp, basque-bdt, belarusian-hse, bulgarian-btb, catalan-ancora, chinese-gsd, coptic-scriptorium, croatian-set, czech-cac, czech-cltt, czech-fictree, czech-pdt, danish-ddt, dutch-alpino, dutch-lassysmall, english-ewt, english-gum, english-lines, english-partut, estonian-edt, finnish-ftb, finnish-tdt, french-gsd, french-partut, french-sequoia, french-spoken, galician-ctg, galician-treegal, german-gsd, gothic-proiel, greek-gdt, hebrew-htb, hindi-hdtb, hungarian-szeged, indonesian-gsd, irish-idt, italian-isdt, italian-partut, italian-postwita, japanese-gsd, korean-gsd, korean-kaist, latin-ittb, latin-perseus, latin-proiel, latvian-lvtb, lithuanian-hse, maltese-mudt, marathi-ufal, north_sami-giella, norwegian-bokmaal, norwegian-nynorsk, norwegian-nynorsklia, old_church_slavonic-proiel, old_french-srcmf, persian-seraji, polish-lfg, polish-sz, portuguese-bosque, portuguese-gsd, romanian-nonstandard, romanian-rrt, russian-gsd, russian-syntagrus, russian-taiga, serbian-set, slovak-snk, slovenian-ssj, slovenian-sst, spanish-ancora, spanish-gsd, swedish-lines, swedish-talbanken, tamil-ttb, telugu-mtg, turkish-imst, ukrainian-iu, urdu, uyghur, vietnamese-vtb}
#'       \item{license: CC-BY-SA-NC}
#'     } 
#'   \item 'jwijffels/udpipe.models.ud.2.0': \url{https://github.com/jwijffels/udpipe.models.ud.2.0}
#'     \itemize{
#'       \item{UDPipe models constructed on data from Universal Dependencies 2.0}
#'       \item{languages-treebanks: ancient_greek-proiel, ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, czech-cac, czech-cltt, czech, danish, dutch-lassysmall, dutch, english-lines, english-partut, english, estonian, finnish-ftb, finnish, french-partut, french-sequoia, french, galician-treegal, galician, german, gothic, greek, hebrew, hindi, hungarian, indonesian, irish, italian, japanese, kazakh, korean, latin-ittb, latin-proiel, latin, latvian, lithuanian, norwegian-bokmaal, norwegian-nynorsk, old_church_slavonic, persian, polish, portuguese-br, portuguese, romanian, russian-syntagrus, russian, sanskrit, slovak, slovenian-sst, slovenian, spanish-ancora, spanish, swedish-lines, swedish, tamil, turkish, ukrainian, urdu, uyghur, vietnamese}
#'       \item{license: CC-BY-SA-NC}
#'     } 
#'   \item 'jwijffels/udpipe.models.conll18.baseline': \url{https://github.com/jwijffels/udpipe.models.conll18.baseline}
#'     \itemize{
#'       \item{UDPipe models constructed on data from Universal Dependencies 2.2}
#'       \item{languages-treebanks: afrikaans-afribooms, ancient_greek-perseus, ancient_greek-proiel, arabic-padt, armenian-armtdp, basque-bdt, bulgarian-btb, buryat-bdt, catalan-ancora, chinese-gsd, croatian-set, czech-cac, czech-fictree, czech-pdt, danish-ddt, dutch-alpino, dutch-lassysmall, english-ewt, english-gum, english-lines, estonian-edt, finnish-ftb, finnish-tdt, french-gsd, french-sequoia, french-spoken, galician-ctg, galician-treegal, german-gsd, gothic-proiel, greek-gdt, hebrew-htb, hindi-hdtb, hungarian-szeged, indonesian-gsd, irish-idt, italian-isdt, italian-postwita, japanese-gsd, kazakh-ktb, korean-gsd, korean-kaist, kurmanji-mg, latin-ittb, latin-perseus, latin-proiel, latvian-lvtb, mixed, north_sami-giella, norwegian-bokmaal, norwegian-nynorsk, norwegian-nynorsklia, old_church_slavonic-proiel, old_french-srcmf, persian-seraji, polish-lfg, polish-sz, portuguese-bosque, romanian-rrt, russian-syntagrus, russian-taiga, serbian-set, slovak-snk, slovenian-ssj, slovenian-sst, spanish-ancora, swedish-lines, swedish-talbanken, turkish-imst, ukrainian-iu, upper_sorbian-ufal, urdu, uyghur, vietnamese-vtb}
#'       \item{license: CC-BY-SA-NC}
#'     } 
#'   \item 'bnosac/udpipe.models.ud': \url{https://github.com/bnosac/udpipe.models.ud}
#'     \itemize{
#'       \item{UDPipe models constructed on data from Universal Dependencies 2.1}
#'       \item{This repository contains models build with this R package on open data from Universal Dependencies 2.1 which allows for commercial usage. The license of these models is mostly CC-BY-SA. Visit that github repository for details on the licenses of the language of your choice. And contact www.bnosac.be if you need support on these models or require models tuned to your needs.}
#'       \item{languages-treebanks: afrikaans, croatian, czech-cac, dutch, english, finnish, french-sequoia, irish, norwegian-bokmaal, persian, polish, portuguese, romanian, serbian, slovak, spanish-ancora, swedish}
#'       \item{license: license is treebank-specific but mainly CC-BY-SA and GPL-3 and LGPL-LR}
#'     } 
#'   \item{If you need to train models yourself for commercial purposes or if you want to improve models, you can easily do this with \code{\link{udpipe_train}} which is explained in detail in the package vignette.}
#' } 
#' Note that when you download these models, you comply to the license of your specific language model.
#' @references 
#' \url{https://ufal.mff.cuni.cz/udpipe}, 
#' \url{https://github.com/jwijffels/udpipe.models.ud.2.3}, 
#' \url{https://github.com/jwijffels/udpipe.models.conll18.baseline}
#' \url{https://github.com/jwijffels/udpipe.models.ud.2.0}, 
#' \url{https://github.com/bnosac/udpipe.models.ud}
#' @export
#' @examples 
#' \dontrun{
#' x <- udpipe_download_model(language = "dutch-alpino")
#' x <- udpipe_download_model(language = "dutch-lassysmall")
#' x <- udpipe_download_model(language = "russian")
#' x <- udpipe_download_model(language = "french")
#' x <- udpipe_download_model(language = "english-partut")
#' x <- udpipe_download_model(language = "english-ewt")
#' x <- udpipe_download_model(language = "german-gsd")
#' x <- udpipe_download_model(language = "spanish-gsd")
#' x <- udpipe_download_model(language = "spanish-gsd", overwrite = FALSE)
#' 
#' x <- udpipe_download_model(language = "english", udpipe_model_repo = "bnosac/udpipe.models.ud")
#' x <- udpipe_download_model(language = "dutch", udpipe_model_repo = "bnosac/udpipe.models.ud")
#' x <- udpipe_download_model(language = "afrikaans", udpipe_model_repo = "bnosac/udpipe.models.ud")
#' x <- udpipe_download_model(language = "spanish-ancora", 
#'                            udpipe_model_repo = "bnosac/udpipe.models.ud")
#' x <- udpipe_download_model(language = "english", 
#'                            udpipe_model_repo = "jwijffels/udpipe.models.conll18.baseline")
#' }
#' x <- udpipe_download_model(language = "sanskrit", 
#'                            udpipe_model_repo = "jwijffels/udpipe.models.ud.2.0", 
#'                            model_dir = tempdir())
#' x
#' x$file_model
#' 
#' ## cleanup for CRAN
#' file.remove(x$file_model)
udpipe_download_model <- function(language = c("afrikaans-afribooms", "ancient_greek-perseus", "ancient_greek-proiel", 
                                               "arabic-padt", "armenian-armtdp", "basque-bdt", "belarusian-hse", 
                                               "bulgarian-btb", "buryat-bdt", "catalan-ancora", "chinese-gsd", 
                                               "coptic-scriptorium", "croatian-set", "czech-cac", "czech-cltt", 
                                               "czech-fictree", "czech-pdt", "danish-ddt", "dutch-alpino", "dutch-lassysmall", 
                                               "english-ewt", "english-gum", "english-lines", "english-partut", 
                                               "estonian-edt", "finnish-ftb", "finnish-tdt", "french-gsd", "french-partut", 
                                               "french-sequoia", "french-spoken", "galician-ctg", "galician-treegal", 
                                               "german-gsd", "gothic-proiel", "greek-gdt", "hebrew-htb", "hindi-hdtb", 
                                               "hungarian-szeged", "indonesian-gsd", "irish-idt", "italian-isdt", 
                                               "italian-partut", "italian-postwita", "japanese-gsd", "kazakh-ktb", 
                                               "korean-gsd", "korean-kaist", "kurmanji-mg", "latin-ittb", "latin-perseus", 
                                               "latin-proiel", "latvian-lvtb", "lithuanian-hse", "maltese-mudt", 
                                               "marathi-ufal", "north_sami-giella", "norwegian-bokmaal", "norwegian-nynorsk", 
                                               "norwegian-nynorsklia", "old_church_slavonic-proiel", "old_french-srcmf", 
                                               "persian-seraji", "polish-lfg", "polish-sz", "portuguese-bosque", 
                                               "portuguese-br", "portuguese-gsd", "romanian-nonstandard", "romanian-rrt", 
                                               "russian-gsd", "russian-syntagrus", "russian-taiga", "sanskrit-ufal", 
                                               "serbian-set", "slovak-snk", "slovenian-ssj", "slovenian-sst", 
                                               "spanish-ancora", "spanish-gsd", "swedish-lines", "swedish-talbanken", 
                                               "tamil-ttb", "telugu-mtg", "turkish-imst", "ukrainian-iu", "upper_sorbian-ufal", 
                                               "urdu-udtb", "uyghur-udt", "vietnamese-vtb"),
                                  model_dir = getwd(),
                                  udpipe_model_repo = c("jwijffels/udpipe.models.ud.2.3", 
                                                        "jwijffels/udpipe.models.ud.2.0", 
                                                        "jwijffels/udpipe.models.conll18.baseline", 
                                                        "bnosac/udpipe.models.ud"), 
                                  overwrite = TRUE,
                                  ...) {
  udpipe_model_repo <- match.arg(udpipe_model_repo)
  if(length(language) > 1){
    stop('You can only provide 1 language to download')
  }
  known_models <- list()
  known_models[["jwijffels/udpipe.models.ud.2.0"]] <- c("ancient_greek-proiel", 
                                                        "ancient_greek", "arabic", "basque", "belarusian", "bulgarian", 
                                                        "catalan", "chinese", "coptic", "croatian", "czech-cac", "czech-cltt", 
                                                        "czech", "danish", "dutch-lassysmall", "dutch", "english-lines", 
                                                        "english-partut", "english", "estonian", "finnish-ftb", "finnish", 
                                                        "french-partut", "french-sequoia", "french", "galician-treegal", 
                                                        "galician", "german", "gothic", "greek", "hebrew", "hindi", "hungarian", 
                                                        "indonesian", "irish", "italian", "japanese", "kazakh", "korean", 
                                                        "latin-ittb", "latin-proiel", "latin", "latvian", "lithuanian", 
                                                        "norwegian-bokmaal", "norwegian-nynorsk", "old_church_slavonic", 
                                                        "persian", "polish", "portuguese-br", "portuguese", "romanian", 
                                                        "russian-syntagrus", "russian", "sanskrit", "slovak", "slovenian-sst", 
                                                        "slovenian", "spanish-ancora", "spanish", "swedish-lines", "swedish", 
                                                        "tamil", "turkish", "ukrainian", "urdu", "uyghur", "vietnamese")
  known_models[["bnosac/udpipe.models.ud"]] <- c("afrikaans", "croatian", "czech-cac", 
                                                 "dutch", "english", "finnish", "french-sequoia", "irish", "norwegian-bokmaal", 
                                                 "persian", "polish", "portuguese", "romanian", "serbian", "slovak", 
                                                 "spanish-ancora", "swedish")
  known_models[["jwijffels/udpipe.models.conll18.baseline"]] <- c("afrikaans-afribooms", 
                                                                  "ancient_greek-perseus", "ancient_greek-proiel", "arabic-padt", 
                                                                  "armenian-armtdp", "basque-bdt", "bulgarian-btb", "buryat-bdt", 
                                                                  "catalan-ancora", "chinese-gsd", "croatian-set", "czech-cac", 
                                                                  "czech-fictree", "czech-pdt", "danish-ddt", "dutch-alpino", "dutch-lassysmall", 
                                                                  "english-ewt", "english-gum", "english-lines", "estonian-edt", 
                                                                  "finnish-ftb", "finnish-tdt", "french-gsd", "french-sequoia", 
                                                                  "french-spoken", "galician-ctg", "galician-treegal", "german-gsd", 
                                                                  "gothic-proiel", "greek-gdt", "hebrew-htb", "hindi-hdtb", "hungarian-szeged", 
                                                                  "indonesian-gsd", "irish-idt", "italian-isdt", "italian-postwita", 
                                                                  "japanese-gsd", "kazakh-ktb", "korean-gsd", "korean-kaist", "kurmanji-mg", 
                                                                  "latin-ittb", "latin-perseus", "latin-proiel", "latvian-lvtb", 
                                                                  "mixed", "north_sami-giella", "norwegian-bokmaal", "norwegian-nynorsk", 
                                                                  "norwegian-nynorsklia", "old_church_slavonic-proiel", "old_french-srcmf", 
                                                                  "persian-seraji", "polish-lfg", "polish-sz", "portuguese-bosque", 
                                                                  "romanian-rrt", "russian-syntagrus", "russian-taiga", "serbian-set", 
                                                                  "slovak-snk", "slovenian-ssj", "slovenian-sst", "spanish-ancora", 
                                                                  "swedish-lines", "swedish-talbanken", "turkish-imst", "ukrainian-iu", 
                                                                  "upper_sorbian-ufal", "urdu", "uyghur", "vietnamese-vtb")
  known_models[["jwijffels/udpipe.models.ud.2.3"]] <- c("afrikaans-afribooms", 
                                                        "ancient_greek-perseus", "ancient_greek-proiel", "arabic-padt", 
                                                        "armenian-armtdp", "basque-bdt", "belarusian-hse", "bulgarian-btb", 
                                                        "catalan-ancora", "chinese-gsd", "coptic-scriptorium", "croatian-set", 
                                                        "czech-cac", "czech-cltt", "czech-fictree", "czech-pdt", "danish-ddt", 
                                                        "dutch-alpino", "dutch-lassysmall", "english-ewt", "english-gum", 
                                                        "english-lines", "english-partut", "estonian-edt", "finnish-ftb", 
                                                        "finnish-tdt", "french-gsd", "french-partut", "french-sequoia", 
                                                        "french-spoken", "galician-ctg", "galician-treegal", "german-gsd", 
                                                        "gothic-proiel", "greek-gdt", "hebrew-htb", "hindi-hdtb", "hungarian-szeged", 
                                                        "indonesian-gsd", "irish-idt", "italian-isdt", "italian-partut", 
                                                        "italian-postwita", "japanese-gsd", "korean-gsd", "korean-kaist", 
                                                        "latin-ittb", "latin-perseus", "latin-proiel", "latvian-lvtb", 
                                                        "lithuanian-hse", "maltese-mudt", "marathi-ufal", "north_sami-giella", 
                                                        "norwegian-bokmaal", "norwegian-nynorsk", "norwegian-nynorsklia", 
                                                        "old_church_slavonic-proiel", "old_french-srcmf", "persian-seraji", 
                                                        "polish-lfg", "polish-sz", "portuguese-bosque", "portuguese-gsd", 
                                                        "romanian-nonstandard", "romanian-rrt", "russian-gsd", "russian-syntagrus", 
                                                        "russian-taiga", "serbian-set", "slovak-snk", "slovenian-ssj", 
                                                        "slovenian-sst", "spanish-ancora", "spanish-gsd", "swedish-lines", 
                                                        "swedish-talbanken", "tamil-ttb", "telugu-mtg", "turkish-imst", 
                                                        "ukrainian-iu", "urdu", "uyghur", "vietnamese-vtb")

  udpipe_defaults_v0_7 <- c("afrikaans", "ancient_greek-proiel", "ancient_greek", "arabic", "basque", 
           "belarusian", "bulgarian", "catalan", "chinese", "coptic", "croatian", 
           "czech-cac", "czech-cltt", "czech", "danish", "dutch-lassysmall", 
           "dutch", "english-lines", "english-partut", "english", "estonian", 
           "finnish-ftb", "finnish", "french-partut", "french-sequoia", 
           "french", "galician-treegal", "galician", "german", "gothic", 
           "greek", "hebrew", "hindi", "hungarian", "indonesian", "irish", 
           "italian", "japanese", "kazakh", "korean", "latin-ittb", "latin-proiel", 
           "latin", "latvian", "lithuanian", "norwegian-bokmaal", "norwegian-nynorsk", 
           "old_church_slavonic", "persian", "polish", "portuguese-br", 
           "portuguese", "romanian", "russian-syntagrus", "russian", "sanskrit", "serbian",
           "slovak", "slovenian-sst", "slovenian", "spanish-ancora", "spanish", 
           "swedish-lines", "swedish", "tamil", "turkish", "ukrainian", 
           "urdu", "uyghur", "vietnamese")
  
  ##
  ## some treebanks were renamed since UD2.2, they now always have an extension
  ##
  treebank_renaming <- list()
  treebank_renaming$v2_1 <- c("afrikaans", "ancient_greek", "arabic", "basque", "belarusian", 
                              "bulgarian", "catalan", "chinese", "coptic", "croatian", "czech", 
                              "danish", "dutch", "english", "estonian", "finnish", "french", 
                              "galician", "german", "gothic", "greek", "hebrew", "hindi", "hungarian", 
                              "indonesian", "irish", "italian", "japanese", 
                              "kazakh", ## NOTE - for this treebank no longer model was built on 2.3, treebank was renamed to kazakh-ktb
                              "korean", "latin", "latvian", "lithuanian", "old_church_slavonic", "persian", 
                              "polish", 
                              #"portuguese-br", ## treebank no longer exists, existed in ud 2.0 (brazilian) but renamed to portuguese-gsd 
                              "portuguese", "romanian", "russian", 
                              "sanskrit", ## NOTE - for this treebank no longer model was built on 2.3, treebank was renamed to sanskrit-ufal
                              "serbian", "slovak", "slovenian", "spanish", "swedish", 
                              "tamil", "turkish", "ukrainian", "urdu", "uyghur", "vietnamese")
  treebank_renaming$v2_2 <- c("afrikaans-afribooms", "ancient_greek-perseus", "arabic-padt", "basque-bdt", "belarusian-hse", 
                              "bulgarian-btb", "catalan-ancora", "chinese-gsd", "coptic-scriptorium", "croatian-set", "czech-pdt", 
                              "danish-ddt", "dutch-alpino", "english-ewt", "estonian-edt", "finnish-tdt", "french-gsd", 
                              "galician-ctg", "german-gsd", "gothic-proiel", "greek-gdt", "hebrew-htb", "hindi-hdtb", "hungarian-szeged", 
                              "indonesian-gsd", "irish-idt", "italian-isdt", "japanese-gsd", 
                              "kazakh-ktb", ## See NOTE above
                              "korean-gsd", "latin-perseus", "latvian-lvtb", "lithuanian-hse", "old_church_slavonic-proiel", "persian-seraji", 
                              "polish-sz", 
                              #"portuguese-br", ## See NOTE above
                              "portuguese-bosque", "romanian-rrt", "russian-gsd", 
                              "sanskrit-ufal", ## See NOTE above
                              "serbian-set", "slovak-snk", "slovenian-ssj", "spanish-gsd", "swedish-talbanken", 
                              "tamil-ttb", "turkish-imst", "ukrainian-iu", "urdu-udtb", "uyghur-udt", "vietnamese-vtb")
  if(!language %in% unlist(known_models)){
    stop("Please provide a valid language.")
  }
  ## language is now uses always UD2.2 treebank names
  language <- txt_recode(language, from = treebank_renaming$v2_1, to = treebank_renaming$v2_2)
  
  if(udpipe_model_repo %in% c("jwijffels/udpipe.models.ud.2.0", "bnosac/udpipe.models.ud")){
    ## If people use jwijffels/udpipe.models.ud.2.0 (UD 2.0) or bnosac/udpipe.models.ud (UD 2.1), recode to UD2.3 name to UD2.1 name
    language <- txt_recode(language, from = treebank_renaming$v2_2, to = treebank_renaming$v2_1)
  }
  if(!language %in% known_models[[udpipe_model_repo]]){
    warning(sprintf("Language %s is not in the list of known languages for which we know a model exists in repository %s", language, udpipe_model_repo))
  }
  if(!dir.exists(model_dir)){
    dir.create(model_dir, recursive = TRUE)  
  }
  if(udpipe_model_repo == "jwijffels/udpipe.models.ud.2.3"){
    filename <- sprintf("%s-ud-2.3-181115.udpipe", language)
    url <- file.path("https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.3/master",
                     "inst", "udpipe-ud-2.3-181115",
                     filename)
    to <- file.path(model_dir, filename)
  }else if(udpipe_model_repo == "jwijffels/udpipe.models.ud.2.0"){
    filename <- sprintf("%s-ud-2.0-170801.udpipe", language)
    url <- file.path("https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.0/master",
                     "inst", "udpipe-ud-2.0-170801",
                     filename)
    to <- file.path(model_dir, filename)
  }else if(udpipe_model_repo == "jwijffels/udpipe.models.conll18.baseline"){
    filename <- sprintf("%s-ud-2.2-conll18-180430.udpipe", language)
    url <- file.path("https://raw.githubusercontent.com/jwijffels/udpipe.models.conll18.baseline/master",
                     "inst", "models", 
                     filename)
    to <- file.path(model_dir, filename)
  }else if(udpipe_model_repo == "bnosac/udpipe.models.ud"){
    filename <- sprintf("%s-ud-2.1-20180111.udpipe", language)
    url <- file.path("https://raw.githubusercontent.com/bnosac/udpipe.models.ud/master", 
                     "models",
                     filename)
    to <- file.path(model_dir, filename)
  }
  if(overwrite || !file.exists(to)){
    message(sprintf("Downloading udpipe model from %s to %s", url, to))
    utils::download.file(url = url, destfile = to, mode = "wb")  
  }
  data.frame(language = language,
             file_model = to,
             url = url,
             stringsAsFactors = FALSE)
}



#' @title Load an UDPipe model
#' @description Load an UDPipe model so that it can be use in \code{\link{udpipe_annotate}}
#' @param file full path to the model or the value returned by a call to \code{\link{udpipe_download_model}}
#' @return An object of class \code{udpipe_model} which is a list with 2 elements
#' \itemize{
#'  \item{file: }{The path to the model as provided by \code{file}}
#'  \item{model: }{An Rcpp-generated pointer to the loaded model which can be used in \code{\link{udpipe_annotate}}}
#' }
#' @seealso \code{\link{udpipe_annotate}}, \code{\link{udpipe_download_model}}, \code{\link{udpipe_train}}
#' @references \url{https://ufal.mff.cuni.cz/udpipe}, \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}
#' @export
#' @examples 
#' x <- udpipe_download_model(language = "dutch-lassysmall", model_dir = tempdir())
#' x$file_model
#' ud_dutch <- udpipe_load_model(x$file_model)
#' \dontrun{
#' x <- udpipe_download_model(language = "english")
#' x$file_model
#' ud_english <- udpipe_load_model(x$file_model)
#' 
#' x <- udpipe_download_model(language = "hebrew")
#' x$file_model
#' ud_hebrew <- udpipe_load_model(x$file_model)
#' }
udpipe_load_model <- function(file) {
  if(is.data.frame(file) && nrow(file) == 1 && "file_model" %in% colnames(file)){
    file <- file$file_model
  }
  file <- path.expand(file)
  if(!file.exists(file)){
    stop(sprintf("File %s containing the language model does not exist", file))
  }
  ptr <- udp_load_model(file)
  out <- structure(
    list(file = file, model = ptr), 
    class = "udpipe_model")
  .loaded_models[[out$file]] <- out
  out 
}
