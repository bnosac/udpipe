txt_morphological <- function(x){
  id <- NULL
  has_morph <- NULL
  
  morpho <- strsplit(x, split = "\\|")
  morpho <- lapply(morpho, FUN=function(x) list(feats = x))
  morpho <- rbindlist(morpho, idcol = "id")
  ## NA values in morpohological features are set to None=None
  morpho$feats[is.na(morpho$feats)] <- "has_morph=FALSE"
  morpho$key <- gsub("^(.+)=(.+)$", "\\1", morpho$feats)
  morpho$key <- tolower(morpho$key) 
  morpho$value <- gsub("^(.+)=(.+)$", "\\2", morpho$feats)
  morpho <- dcast.data.table(data = morpho, formula = id ~ key, value.var = "value")
  if("has_morph" %in% colnames(morpho)){
    morpho <- morpho[, has_morph := as.logical(has_morph)]
    morpho$has_morph[is.na(morpho$has_morph)] <- TRUE
  }else{
    morpho <- morpho[, has_morph := TRUE]
  }
  morpho <- morpho[, id := NULL]
  morpho <- setcolorder(morpho, c("has_morph", setdiff(colnames(morpho), "has_morph")))
  morpho <- setDF(morpho)
  morpho
} 