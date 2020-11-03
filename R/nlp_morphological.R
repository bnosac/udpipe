#' @title Add morphological features to an annotated dataset
#' @description The result of \code{\link{udpipe_annotate}} which is put into a data.frame 
#' returns a field called \code{feats} containing morphological features as defined at 
#' \url{https://universaldependencies.org/u/feat/index.html}. If there are several of these features,
#' these are concatenated with the \code{|} symbol. This function extracts each of these morphological 
#' features separately and adds these as extra columns to the data.frame
#' @param x a data.frame or data.table as returned by \code{as.data.frame(udpipe_annotate(...))}
#' @param term the name of the field in \code{x} which contains the morphological features. Defaults to 'feats'.
#' @param which a character vector with names of morphological features to uniquely parse out. These 
#' features are one of the 24 lexical and grammatical properties of words defined at \url{https://universaldependencies.org/u/feat/index.html}.
#' Possible values are:
#' \itemize{
#' \item{"lexical": "PronType", "NumType", "Poss", "Reflex", "Foreign", "Abbr", "Typo"}
#' \item{"inflectional_noun": "Gender", "Animacy", "NounClass", "Number", "Case", "Definite", "Degree"}
#' \item{"inflectional_verb": "VerbForm", "Mood", "Tense", "Aspect", "Voice", "Evident", "Polarity", "Person", "Polite", "Clusivity"}
#' }
#' See the examples.
#' @return \code{x} in the same order with extra columns added (at least the column has_morph is added indicating
#' if any morphological features are present and as well extra columns for each possible morphological feature in the data)
#' @export
#' @examples 
#' \dontrun{
#' udmodel <- udpipe_download_model(language = "english-ewt")
#' udmodel <- udpipe_load_model(file = udmodel$file_model)
#' x <- udpipe_annotate(udmodel, 
#'                      x = "The economy is weak but the outlook is bright")
#' x <- as.data.frame(x)
#' x <- cbind_morphological(x, term = "feats")
#' }
#' 
#' f <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
#' x <- udpipe_read_conllu(f)
#' x <- cbind_morphological(x, term = "feats")
#' 
#' f <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
#' x <- udpipe_read_conllu(f)
#' x <- cbind_morphological(x, term = "feats", 
#'                          which = c("Mood", "Gender", "VerbForm", "Polarity", "Polite"))
#' 
#' # extract all features from the feats column even if not present in the data
#' f <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
#' x <- udpipe_read_conllu(f)
#' x <- cbind_morphological(x, term = "feats", 
#'                          which = c("lexical", "inflectional_noun", "inflectional_verb"))
cbind_morphological <- function(x, term = "feats", which){
  stopifnot(inherits(x, "data.frame"))
  stopifnot(length(term) == 1)
  stopifnot(term %in% colnames(x))
  oldfields <- colnames(x)
  out <- txt_morphological(x[[term]])
  out <- data.table::setnames(out, 
                              old = colnames(out), 
                              new = c("has_morph", sprintf("morph_%s", setdiff(colnames(out), "has_morph"))))
  newfields <- setdiff(colnames(out), c("has_morph", oldfields))
  if(inherits(x, "data.table")){
    x <- setDF(x)
    x[, colnames(out)] <- out  
    x <- setDT(x)
  }else if(inherits(x, "data.frame")){
    x[, colnames(out)] <- out  
  }
  
  if(!missing(which)){
    morpho_feats <- list()
    morpho_feats$lexical <- c("PronType", "NumType", "Poss", "Reflex", "Foreign", "Abbr", "Typo")
    morpho_feats$inflectional_noun <- c("Gender", "Animacy", "NounClass", "Number", "Case", "Definite", "Degree")
    morpho_feats$inflectional_verb <- c("VerbForm", "Mood", "Tense", "Aspect", "Voice", "Evident", "Polarity", "Person", "Polite", "Clusivity")
    if(any(!which %in% c("lexical", "inflectional_noun", "inflectional_verb"))){
      morpho_feats <- as.character(which)
    }else{
      morpho_feats <- c(unlist(morpho_feats[c("lexical", "inflectional_noun", "inflectional_verb") %in% which]), setdiff(which, c("lexical", "inflectional_noun", "inflectional_verb"))) 
    }
    morpho_feats <- tolower(morpho_feats)
    morpho_feats <- sprintf("morph_%s", morpho_feats)
    missing <- setdiff(morpho_feats, colnames(x))
    if(inherits(x, "data.table")){
      for(field in missing){
        x[, eval(field) := NA_character_]  
      }
      x <- x[, unique(c(oldfields, "has_morph", morpho_feats)), drop = FALSE]
      x <- data.table::setcolorder(x, neworder = unique(c(oldfields, "has_morph", morpho_feats)))
    }else if(inherits(x, "data.frame")){
      x <- data.table::setDT(x)
      for(field in missing){
        x[, eval(field) := NA_character_]  
      }
      x <- data.table::setDF(x)
      x <- x[, unique(c(oldfields, "has_morph", morpho_feats)), drop = FALSE]
      x <- data.table::setDT(x)
      x <- data.table::setcolorder(x, neworder = unique(c(oldfields, "has_morph", morpho_feats)))
      x <- data.table::setDF(x)
    }
  }
  x
}



txt_morphological <- function(x){
  ## R CMD check happiness
  id <- has_morph <- NULL
  
  ## Split field according to |, next put it into data.frame and reshape it to wide format
  ## Add an indicator has_morph indicating if morphological features are uberhaupt present
  morpho <- strsplit(x, split = "\\|")
  morpho <- lapply(morpho, FUN=function(x) list(feats = x))
  morpho <- data.table::rbindlist(morpho, idcol = "id")
  ## NA values in morpohological features are set to None=None
  morpho$feats[is.na(morpho$feats)] <- "has_morph=FALSE"
  morpho$key <- gsub("^(.+)=(.+)$", "\\1", morpho$feats)
  morpho$key <- tolower(morpho$key) 
  morpho$value <- gsub("^(.+)=(.+)$", "\\2", morpho$feats)
  morpho <- data.table::dcast.data.table(data = morpho, formula = id ~ key, value.var = "value")
  if("has_morph" %in% colnames(morpho)){
    morpho <- morpho[, has_morph := as.logical(has_morph)]
    morpho$has_morph[is.na(morpho$has_morph)] <- TRUE
  }else{
    morpho <- morpho[, has_morph := TRUE]
  }
  morpho <- morpho[, id := NULL]
  morpho <- data.table::setcolorder(morpho, c("has_morph", setdiff(colnames(morpho), "has_morph")))
  morpho <- data.table::setDF(morpho)
  morpho
} 



