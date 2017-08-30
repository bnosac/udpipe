#' @title Download an UDPipe model provided by the UDPipe community for a specific language of choice
#' @description 
#' Ready-made models for 50 languages trained on 67 treebanks are provided by UDPipe at https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364
#' in one zip file. You can either download these manually in order to use it for annotation purposes 
#' or use \code{udpipe_download_model} to download these models for a specific language of choice. \cr
#' 
#' For your convenience, these models are also made available at https://github.com/jwijffels/udpipe.models.ud.2.0 under the CC-BY-NC-SA
#' licence. This function downloads the models from that location, so if you use this function you are complying to that license.
#' If you want to train models for commercial purposes, you can easily do this with \code{\link{udpipe_train}}
#' @param language a character stirng with a language. Possible values are:
#' ancient_greek-proiel, ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, 
#' czech-cac, czech-cltt, czech, danish, dutch-lassysmall, dutch, english-lines, english-partut, english, estonian, 
#' finnish-ftb, finnish, french-partut, french-sequoia, french, galician-treegal, galician, german, gothic, 
#' greek, hebrew, hindi, hungarian, indonesian, irish, italian, japanese, kazakh, korean, latin-ittb, latin-proiel, 
#' latin, latvian, lithuanian, norwegian-bokmaal, norwegian-nynorsk, 
#' old_church_slavonic, persian, polish, portuguese-br, 
#' portuguese, romanian, russian-syntagrus, russian, sanskrit, slovak, slovenian-sst, slovenian, spanish-ancora, spanish, 
#' swedish-lines, swedish, tamil, turkish, ukrainian, 
#' urdu, uyghur, vietnamese
#' @param model_dir a path where the model will be downloaded to. Defaults to the current working directory
#' @param ... other arguments passed on to \code{\link{download.file}}
#' @return A data.frame with 1 row and 2 columns: language and model_file which is the location where the model was downloaded to
#' @seealso \code{\link{udpipe_load_model}}
#' @details Pre-trained Universal Dependencies 2.0 models on all UD treebanks are made available at 
#' \url{https://ufal.mff.cuni.cz/udpipe}, namely at \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}.
#' At the time of writing this consists of models made available on 50 languages, namely: 
#' ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, czech, danish, dutch, 
#' english, estonian, finnish, french, galician, german, gothic, greek, hebrew, hindi, hungarian, indonesian, irish, 
#' italian, japanese, kazakh, korean, latin, latvian, lithuanian, norwegian, old_church_slavonic, persian, polish, 
#' portuguese, romanian, russian, sanskrit, slovak, slovenian, spanish, swedish, tamil, turkish, ukrainian, 
#' urdu, uyghur, vietnamese. Mark that these models are made available under the CC BY-NC-SA 4.0 license. \cr
#' 
#' These models are also provided in an R package for your convenience at 
#' \url{https://github.com/jwijffels/udpipe.models.ud.2.0}
#' @references \url{https://ufal.mff.cuni.cz/udpipe}, \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}
#' @export
#' @examples 
#' \dontrun{
#' x <- udpipe_download_model(language = "dutch")
#' x
#' x <- udpipe_download_model(language = "dutch-lassysmall")
#' x <- udpipe_download_model(language = "sanskrit")
#' x <- udpipe_download_model(language = "russian")
#' x <- udpipe_download_model(language = "french")
#' x <- udpipe_download_model(language = "english")
#' x <- udpipe_download_model(language = "german")
#' x <- udpipe_download_model(language = "spanish")
#' }
udpipe_download_model <- function(language = c("ancient_greek-proiel", "ancient_greek", "arabic", "basque", 
                                               "belarusian", "bulgarian", "catalan", "chinese", "coptic", "croatian", 
                                               "czech-cac", "czech-cltt", "czech", "danish", "dutch-lassysmall", 
                                               "dutch", "english-lines", "english-partut", "english", "estonian", 
                                               "finnish-ftb", "finnish", "french-partut", "french-sequoia", 
                                               "french", "galician-treegal", "galician", "german", "gothic", 
                                               "greek", "hebrew", "hindi", "hungarian", "indonesian", "irish", 
                                               "italian", "japanese", "kazakh", "korean", "latin-ittb", "latin-proiel", 
                                               "latin", "latvian", "lithuanian", "norwegian-bokmaal", "norwegian-nynorsk", 
                                               "old_church_slavonic", "persian", "polish", "portuguese-br", 
                                               "portuguese", "romanian", "russian-syntagrus", "russian", "sanskrit", 
                                               "slovak", "slovenian-sst", "slovenian", "spanish-ancora", "spanish", 
                                               "swedish-lines", "swedish", "tamil", "turkish", "ukrainian", 
                                               "urdu", "uyghur", "vietnamese"),
                                  model_dir = getwd(),
                                  ...) {
  language <- match.arg(language)
  if(!dir.exists(model_dir)){
    dir.create(model_dir, recursive = TRUE)  
  }
  filename <- sprintf("%s-ud-2.0-170801.udpipe", language)
  url <- file.path("https://github.com/jwijffels/udpipe.models.ud.2.0/raw/master",
                   "inst", "udpipe-ud-2.0-170801",
                   filename)
  to <- file.path(model_dir, filename)
  utils::download.file(url = url, destfile = to, ...)
  data.frame(language = language,
             file_model = to,
             stringsAsFactors = FALSE)
}



#' @title Load an UDPipe model
#' @description Load an UDPipe model
#' @param file full path to the model
#' @return An object of class \code{udpipe_model} which is a list with 2 elements: file containing the file and model which is 
#' an Rcpp-generated pointer to the loaded model which can be used in \code{\link{udpipe_annotate}}
#' @seealso \code{\link{udpipe_annotate}}
#' @details Pre-trained Universal Dependencies 2.0 models on all UD treebanks are made available at 
#' \url{https://ufal.mff.cuni.cz/udpipe}, namely at \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}.
#' At the time of writing this consists of models made available on 50 languages, namely: 
#' ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, czech, danish, dutch, 
#' english, estonian, finnish, french, galician, german, gothic, greek, hebrew, hindi, hungarian, indonesian, irish, 
#' italian, japanese, kazakh, korean, latin, latvian, lithuanian, norwegian, old_church_slavonic, persian, polish, 
#' portuguese, romanian, russian, sanskrit, slovak, slovenian, spanish, swedish, tamil, turkish, ukrainian, 
#' urdu, uyghur, vietnamese. Mark that these models are made available under the CC BY-NC-SA 4.0 license. \cr
#' 
#' These models are also provided in an R package for your convenience at 
#' \url{https://github.com/jwijffels/udpipe.models.ud.2.0}
#' @references \url{https://ufal.mff.cuni.cz/udpipe}, \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}
#' @export
#' @examples 
#' ##
#' ## Ready-made models for 50 languages are provided by UDPipe at
#' ##    https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364
#' ## 
#' 
#' ## Download zipped folder with models build on UD version 2.0 + get one model for Dutch
#' url <- file.path("https://lindat.mff.cuni.cz",
#'  "repository/xmlui/bitstream/handle/11234/1-2364/udpipe-ud-2.0-170801.zip")
#' url
#' \dontrun{
#' download.file(url, "udpipe-ud-2.0-170801.zip")
#' unzip("udpipe-ud-2.0-170801.zip", list = TRUE)
#' unzip("udpipe-ud-2.0-170801.zip", 
#'       files = "udpipe-ud-2.0-170801/dutch-ud-2.0-170801.udpipe")
#' 
#' f <- file.path(getwd(), "udpipe-ud-2.0-170801/dutch-ud-2.0-170801.udpipe")
#' ## Load model
#' ud_dutch <- udpipe_load_model(f)
#' 
#' ##
#' ## These languages are also put into an R package provided at 
#' ##   https://github.com/jwijffels/udpipe.models.ud.2.0
#' ## You can get these as follows
#' install.packages("udpipe.models.ud.2.0", repos = "http://www.datatailor.be/rcube", type = "source")
#' f <- system.file(package = "udpipe.models.ud.2.0", "dutch-ud-2.0-170801.udpipe")
#' ud_dutch <- udpipe_load_model(f)
#' 
#' ## see all models inside that package
#' list.files(system.file(package = "udpipe.models.ud.2.0", "udpipe-ud-2.0-170801"), 
#'   recursive = TRUE, full.names = TRUE)
#' }
udpipe_load_model <- function(file) {
  file <- path.expand(file)
  if(!file.exists(file)){
    stop(sprintf("File %s containing the language model does not exist", file))
  }
  if(basename(file) == file){
    stop(sprintf("You should provide the full path to the file %s, as in %s", file, file.path(getwd(), file)))
  }
  ptr <- udp_load_model(file)
  structure(
    list(file = file, model = ptr), 
    class = "udpipe_model")
}
