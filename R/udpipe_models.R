#' @title Download an UDPipe model provided by the UDPipe community for a specific language of choice
#' @description 
#' Ready-made models for 52 languages trained on 69 treebanks are provided to you.
#' Some or these models were provided by the UDPipe community. Other models were build using this R package.
#' You can either download these models manually in order to use it for annotation purposes 
#' or use \code{udpipe_download_model} to download these models for a specific language of choice. \cr
#' 
#' The models provided by the UDPipe community are made available for your convenience at \url{https://github.com/jwijffels/udpipe.models.ud.2.0} under the CC-BY-NC-SA
#' licence. This function downloads the models by default from that location, so if you use this function you are complying to that license.\cr
#' 
#' If you are working in a commercial setting, you can also choose to download models from \url{https://github.com/bnosac/udpipe.models.ud}. That
#' repository contains models build with this R package on open data which allows for commercial usage. 
#' The license of these models is mostly CC-BY-SA. Visit that github repository for details on the licenses of the language of your choice. 
#' And contact www.bnosac.be if you need support on these models or require models tuned to your needs. \cr
#' 
#' If you need to train models yourself for commercial purposes or if you want to improve models, 
#' you can easily do this with \code{\link{udpipe_train}} which is explained in detail in the package vignette.
#' @param language a character string with a language. Possible values are:
#' afrikaans, ancient_greek-proiel, ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, 
#' czech-cac, czech-cltt, czech, danish, dutch-lassysmall, dutch, english-lines, english-partut, english, estonian, 
#' finnish-ftb, finnish, french-partut, french-sequoia, french, galician-treegal, galician, german, gothic, 
#' greek, hebrew, hindi, hungarian, indonesian, irish, italian, japanese, kazakh, korean, latin-ittb, latin-proiel, 
#' latin, latvian, lithuanian, norwegian-bokmaal, norwegian-nynorsk, 
#' old_church_slavonic, persian, polish, portuguese-br, 
#' portuguese, romanian, russian-syntagrus, russian, sanskrit, serbian, slovak, slovenian-sst, slovenian, spanish-ancora, spanish, 
#' swedish-lines, swedish, tamil, turkish, ukrainian, 
#' urdu, uyghur, vietnamese. \cr \cr
#' \itemize{
#'   \item \code{udpipe_model_repo} 'jwijffels/udpipe.models.ud.2.0' contains models for all above enumerated languages except afrikaans and serbian
#'   \item \code{udpipe_model_repo} 'bnosac/udpipe.models.ud' contains models for the following languages: afrikaans, croatian, czech-cac, dutch, english, finnish, french-sequoia, irish, norwegian-bokmaal, persian, polish, portuguese, romanian, serbian, slovak, spanish-ancora, swedish
#' }
#' @param model_dir a path where the model will be downloaded to. Defaults to the current working directory
#' @param udpipe_model_repo location where the models will be downloaded from. 
#' Either 'jwijffels/udpipe.models.ud.2.0' or 'bnosac/udpipe.models.ud'. \cr
#' Defaults to 'jwijffels/udpipe.models.ud.2.0'. \cr
#' \itemize{
#'   \item 'jwijffels/udpipe.models.ud.2.0' contains models released under the CC-BY-NC-SA license
#'   \item 'bnosac/udpipe.models.ud' contains models mainly released under the CC-BY-SA license
#' }
#' @return A data.frame with 1 row and 3 columns: 
#' \itemize{
#'  \item{language: }{The language as provided by the input parameter \code{language}}
#'  \item{file_model: }{The path to the file on disk where the model was downloaded to}
#'  \item{url: }{The URL where the model was downloaded from}
#' }
#' @seealso \code{\link{udpipe_load_model}}
#' @details 
#' Pre-trained Universal Dependencies 2.0 models on all UD treebanks are made available at 
#' \url{https://ufal.mff.cuni.cz/udpipe}, namely at \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}.
#' At the time of writing this consists of models made available on 50 languages, namely: 
#' ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, czech, danish, dutch, 
#' english, estonian, finnish, french, galician, german, gothic, greek, hebrew, hindi, hungarian, indonesian, irish, 
#' italian, japanese, kazakh, korean, latin, latvian, lithuanian, norwegian, old_church_slavonic, persian, polish, 
#' portuguese, romanian, russian, sanskrit, slovak, slovenian, spanish, swedish, tamil, turkish, ukrainian, 
#' urdu, uyghur, vietnamese. Mark that these models are made available under the CC BY-NC-SA 4.0 license. \cr
#' These models are also provided in an R package for your convenience at 
#' \url{https://github.com/jwijffels/udpipe.models.ud.2.0} \cr 
#' 
#' Pre-trained Universal Dependencies 2.1 models on UD treebanks which allow for commercial usage 
#' (mainly by using data which is released under the CC-BY-SA license, but also some are released under the GPL-3 and LGPL-LR license) 
#' are made available at \url{https://github.com/bnosac/udpipe.models.ud}.
#' At the time of writing this consists of models made available on 17 languages, namely: 
#' afrikaans, croatian, czech-cac, dutch, english, finnish, french-sequoia, irish, norwegian-bokmaal, 
#' persian, polish, portuguese, romanian, serbian, slovak, spanish-ancora, swedish. 
#' Visit that repository for more details on the license of these. 
#' @references \url{https://ufal.mff.cuni.cz/udpipe}, \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364},
#' \url{https://github.com/jwijffels/udpipe.models.ud.2.0}, \url{https://github.com/bnosac/udpipe.models.ud}
#' @export
#' @examples 
#' x <- udpipe_download_model(language = "sanskrit", model_dir = tempdir())
#' x
#' x$file_model
#' \dontrun{
#' x <- udpipe_download_model(language = "dutch")
#' x <- udpipe_download_model(language = "dutch-lassysmall")
#' x <- udpipe_download_model(language = "russian")
#' x <- udpipe_download_model(language = "french")
#' x <- udpipe_download_model(language = "english")
#' x <- udpipe_download_model(language = "german")
#' x <- udpipe_download_model(language = "spanish")
#' 
#' x <- udpipe_download_model(language = "spanish-ancora", udpipe_model_repo = "bnosac/udpipe.models.ud")
#' x <- udpipe_download_model(language = "english", udpipe_model_repo = "bnosac/udpipe.models.ud")
#' x <- udpipe_download_model(language = "dutch", udpipe_model_repo = "bnosac/udpipe.models.ud")
#' x <- udpipe_download_model(language = "afrikaans", udpipe_model_repo = "bnosac/udpipe.models.ud")
#' }
udpipe_download_model <- function(language = c("afrikaans", "ancient_greek-proiel", "ancient_greek", "arabic", "basque", 
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
                                               "urdu", "uyghur", "vietnamese"),
                                  model_dir = getwd(),
                                  udpipe_model_repo = c("jwijffels/udpipe.models.ud.2.0", "bnosac/udpipe.models.ud")) {
  language <- match.arg(language)
  udpipe_model_repo <- match.arg(udpipe_model_repo)
  if(!dir.exists(model_dir)){
    dir.create(model_dir, recursive = TRUE)  
  }
  if(udpipe_model_repo == "jwijffels/udpipe.models.ud.2.0"){
    filename <- sprintf("%s-ud-2.0-170801.udpipe", language)
    url <- file.path("https://github.com/jwijffels/udpipe.models.ud.2.0/raw/master",
                     "inst", "udpipe-ud-2.0-170801",
                     filename)
    url <- file.path("https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.0/master",
                     "inst", "udpipe-ud-2.0-170801",
                     filename)
    to <- file.path(model_dir, filename)
  }else if(udpipe_model_repo == "bnosac/udpipe.models.ud"){
    filename <- sprintf("%s-ud-2.1-20180105.udpipe", language)
    url <- file.path("https://raw.githubusercontent.com/bnosac/udpipe.models.ud/master", 
                     "models",
                     filename)
    to <- file.path(model_dir, filename)
  }
  message(sprintf("Downloading udpipe model from %s to %s", url, to))
  utils::download.file(url = url, destfile = to, mode = "wb")
  data.frame(language = language,
             file_model = to,
             url = url,
             stringsAsFactors = FALSE)
}



#' @title Load an UDPipe model
#' @description Load an UDPipe model so that it can be use in \code{\link{udpipe_annotate}}
#' @param file full path to the model
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
  file <- path.expand(file)
  if(!file.exists(file)){
    stop(sprintf("File %s containing the language model does not exist", file))
  }
  ptr <- udp_load_model(file)
  structure(
    list(file = file, model = ptr), 
    class = "udpipe_model")
}
