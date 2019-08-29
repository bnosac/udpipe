expr_list <- function(...){
  #expr <- substitute(list(...))
  #expr <- as.list(expr[-1])
  expr <- substitute(...())  
  expr
}

expr_labels = function(expr){
  labels <- names(expr)
  if(is.null(labels)){
    labels <- unlist(lapply(expr, FUN=function(e) paste(deparse(e, backtick = TRUE), collapse = "\n")))
  }
  idx <- which(labels == "")
  labels[idx] <- sapply(expr[idx], FUN=function(e) paste(deparse(e, backtick = TRUE), collapse = "\n"), USE.NAMES = FALSE)
  labels      <- unlist(labels)
  as.character(labels)
}








##
## rowLinks class indicating which term is connected to another
##
rowLinks <- setRefClass("rowLinks", fields = list(rowids = "list", data = "data.table", n = "integer"))
rowLinks$methods(list(
  initialize = function(rowids = list()) {
    .self$rowids <- rowids
    .self$n      <- length(.self$rowids)
    .self$data   <- .self$as.data.table(rowids)
    invisible(.self)
  },
  as.data.table = function(x){
    x <- unclass(x)
    DT <- mapply(seq_along(x), x, FUN=function(i, id){
      if(length(id) > 0){
        list(row = i, row_childparent = id)
      }else{
        list(row = integer(0), row_childparent = integer(0))
      }
    }, SIMPLIFY=FALSE)
    DT <- data.table::rbindlist(DT)
    DT
  }))




##
## linkChain class indicating the chain of child/parent links
##
linkChain <- setRefClass("linkChain", fields = list(rowids = "rowLinks", data = "data.table", n = "integer", current_depth = "integer",
                                                    labels = "character",
                                                    last_matched_on = "character"))
linkChain$methods(list(
  initialize = function(rowids) {
    stopifnot(inherits(rowids, "rowLinks"))
    .self$rowids          <- rowids
    .self$n               <- .self$rowids$n
    .self$data            <- .self$rowids$data
    .self$current_depth   <- 0L
    .self$last_matched_on <- character(0)
    .self$labels          <- character(0)
    invisible(.self)
  },
  as.data.table = function(x){
    x <- unclass(x)
    DT <- mapply(seq_along(x), x, FUN=function(i, id){
      if(length(id) > 0){
        list(row = i,          row_childparent = id)
      }else{
        list(row = integer(0), row_childparent = integer(0))
      }
    }, SIMPLIFY=FALSE)
    DT <- data.table::rbindlist(DT)
    DT
  },
  match = function(links, depth, label = paste("row_link_", depth, sep = "")) {
    stopifnot(inherits(links, "list"))
    stopifnot(length(links) == .self$n)
    
    if(missing(depth)){
      .self$current_depth <- .self$current_depth + 1L
      depth <- .self$current_depth
    }
    ## Do the chain operation
    links       <- .self$as.data.table(links)
    links       <- data.table::setnames(links,      old = "row", new = "link")
    .self$data  <- data.table::setnames(.self$data, old = "row_childparent", new = "link")
    .self$data  <- merge(links, .self$data,
                         by.x = "link", by.y = "link", all.x = FALSE, all.y = FALSE, allow.cartesian = TRUE,
                         suffixes = c("newlink", paste("_depth", depth, sep = "")))
    .self$data <- data.table::setnames(.self$data, old = "link", new = label)
    ## Maintain some administration
    .self$last_matched_on <- label
    .self$labels <- c(.self$labels, label)
    .self$current_depth <- as.integer(depth)
    invisible(.self)
  },
  merge = function(chain){
    stopifnot(inherits(chain, "linkChain"))
    .self$current_depth <- .self$current_depth + chain$current_depth + 1L
    depth <- .self$current_depth
    
    if(FALSE){
      .self$data <- combine(.self$data, chain$data, .self$current_depth)
    }else{
      links       <- data.table::setnames(chain$data, old = "row", new = "link")
      .self$data  <- data.table::setnames(.self$data, old = "row_childparent", new = "link")
      ## make sure other names are
      currentlabels <- .self$data$labels
      if(length(currentlabels) > 0){
        newlabels <- paste("row_link_", seq.int(from = chain$data$current_depth + 1, to = chain$data$current_depth + seq_along(currentlabels)), sep = "")
        .self$data   <- data.table::setnames(.self$data, old = currentlabels, new = newlabels)
        .self$labels <- newlabels
      }
      # TODO: need to fix S3 dispatching which seems troublesome with data.table
      .self$data  <- data.table:::merge.data.table(chain$data, .self$data,
                                                   by.x = "link", by.y = "link", all.x = FALSE, all.y = FALSE, #allow.cartesian = TRUE,
                                                   suffixes = c("newlink", paste("_depth", depth, sep = "")))
      newname <- paste("row_link_", depth, sep = "")
      .self$data <- data.table::setnames(.self$data, old = "link", new = newname)
    }
    .self$last_matched_on <- newname
    .self$labels <- c(.self$labels, newname)
    .self$current_depth <- as.integer(depth)
    invisible(.self)
  },
  filter = function(subset, type = c("row_childparent", "row")){
    stopifnot(inherits(subset, "logical"))
    stopifnot(length(subset) == .self$n)
    type <- match.arg(type)
    
    rows <- which(subset)
    if(type == "row"){
      .self$data <- .self$data[.self$data$row %in% rows, ]
    }else if(type == "row_childparent"){
      .self$data <- .self$data[.self$data$row_childparent %in% rows, ]
    }
    invisible(.self)
  },
  AND = function(subset){
    stopifnot(inherits(subset, "logical"))
    stopifnot(length(subset) == .self$n)
    
    .self$filter(subset, type = "row")
  },
  OR = function(subset){
    stopifnot(inherits(subset, "logical"))
    stopifnot(length(subset) == .self$n)
    
    rows         <- which(!subset)
    result       <- .self$rowids$rowids
    result[rows] <- lapply(result[rows], FUN = function(i) integer(length = 0))
    result       <- .self$as.data.table(result)
    result       <- rbindlist(list(.self$data, result), fill = TRUE) ## this prefers the existing links if they are more complex of the simpler ones
    result       <- unique(result, by = c("row", "row_childparent")) ## this requires data.table >= 1.11.0
    ## TODO: this is not correct as row_childparent should be indicated as not usefull yet
    .self$data   <- result
    invisible(.self)
  },
  sequence = function(exclude = character(0)) {
    stopifnot(is.character(exclude))
    #exclude <- c(exclude, ".hidden")
    #exclude <- c("row", "row_childparent")
    only <- setdiff(colnames(.self$data), exclude)
    only <- union(.self$labels, only)
    lapply(seq_len(.self$n), FUN = function(i){
      x <- .self$data[row == i, only, with = FALSE]
      x <- apply(x, MARGIN=1, FUN=function(x) as.list(na.exclude(as.integer(x))))
      x <- lapply(x, unlist)
    })
  },
  links = function(type = c("current")) {
    type <- match.arg(type)
    if(type == "current"){
      lapply(seq_len(.self$n), FUN=function(i){
        x <- .self$data[row == i, row_childparent]
        x
      })
    }
  }
))






##################################
## syntaxrelation
##
syntaxrelation <- setRefClass("syntaxrelation",
                              fields = list(expr = "list", n = "integer", type = "character", invert = "logical"))
has_child <- function(...){
  new("syntaxrelation", ..., type = "child")
}
has_parent <- function(...){
  new("syntaxrelation", ..., type = "parent")
}
syntaxrelation$methods(list(
  initialize = function(..., type = c("default", "child", "parent")) {
    expressions             <- expr_list(...)
    expressionlabels        <- expr_labels(expressions)
    contains_syntaxrelation <- sapply(expressions, FUN = function(e) any(c("relationship", "has_child", "has_parent") %in% all.names(e)), USE.NAMES = FALSE)
    .self$n    <- length(expressions)
    .self$expr <- list(expr = expressions,
                       label = expressionlabels,
                       contains_syntaxrelation = unlist(contains_syntaxrelation))
    .self$invert <- FALSE
    if(length(expressions) > 0){
      overwrite_type <- head(as.character(expressions[[1]]), n = 1)
      if(overwrite_type == "has_child"){
        .self$type <- "child"
      }else if(overwrite_type == "has_child"){
        .self$type <- "parent"
      }else{
        .self$type <- match.arg(type)
      }
    }else{
      .self$type <- match.arg(type)
    }
    .self$type <- match.arg(type)
    invisible(.self)
  },
  has_child_parent_logic = function(){
    any(.self$expr$contains_syntaxrelation)
  },
  show = function(){
    cat(sprintf('has_%s at depth %s', ifelse(length(.self$type) == 0, "unknown", .self$type), .self$depth), sep = "\n")
    cat(paste(sprintf(" - %s", .self$expr$label), collapse = "\n"), sep = "\n")
  },
  rowids = function(DT){
    ## Get for which rows this logic is valid
    if(inherits(DT, "data.table")){
      if(.self$type == "default"){
        result <- replicate(n = nrow(DT), expr = seq_len(nrow(DT)), simplify = FALSE)
      }else if(.self$type == "child"){
        result <- DT$child_rowid
      }else if(.self$type == "parent"){
        result <- DT$parent_rowid
      }
    }else{
      if(.self$type == "default"){
        result <- get("child_rowid", envir = DT)
        result <- replicate(n = length(result), expr = seq_len(length(result)), simplify = FALSE)
      }else if(.self$type == "child"){
        result <- get("child_rowid", envir = DT)
      }else if(.self$type == "parent"){
        result <- get("parent_rowid", envir = DT)
      }
      stopifnot(length(result) > 0)
    }
    unclass(result)
  },
  run = function(DT){
    stopifnot(inherits(DT, c("data.table", "environment")))
    stopifnot(.self$n == 1L) ## THIS IS NO LONGER POSSIBLE DUE TO CHAINING AND COPY SEMANTICS OF DATA.TABLE
    
    chain  <- new("linkChain", new("rowLinks", .self$rowids(DT)))
    
    expression  <- .self$expr$expr[[1]]
    label       <- .self$expr$expr[[1]]
    result      <- eval(expression, DT)
    if(is.logical(result)){
      result <- chain$filter(result)
    }else if(inherits(result, "linkChain")){
      if(.self$type %in% c("child", "parent")){
        result <- chain$merge(result)
      }
    }else if(inherits(result, "syntaxrelation")){
      result <- result$run(DT = DT)
    }
    result
  }))


setMethod("|", signature = c("syntaxrelation", "logical"),
          definition = function(e1, e2) {
            chain <- e1$run(parent.frame())
            chain$OR(e2)
            .NotYetImplemented() ## Give error as long as issue with labels is not solved yet
          })
setMethod("|", signature = c("logical", "syntaxrelation"),
          definition = function(e1, e2) {
            chain <- e2$run(parent.frame())
            chain$OR(e1)
            .NotYetImplemented() ## Give error as long as issue with labels is not solved yet
          })
setMethod("&", signature = c("syntaxrelation", "logical"),
          definition = function(e1, e2) {
            chain <- e1$run(parent.frame())
            chain$AND(e2)
          })
setMethod("&", signature = c("logical", "syntaxrelation"),
          definition = function(e1, e2) {
            chain <- e2$run(parent.frame())
            chain$AND(e1)
          })


syntaxpatterns <- setRefClass("syntaxpatterns",
                              fields = list(data = "data.table", relations = "syntaxrelation"),
                              methods = list(
                                initialize = function(data, ...) {
                                  .self$data        <- data
                                  .self$relations   <- new("syntaxrelation", ...)
                                  if(!.self$relations$has_child_parent_logic()){
                                    stop("This does not make sense, there is no call to has_child or has_parent in your code")
                                  }
                                  invisible(.self)
                                },
                                run = function(){
                                  .self$relations$run(.self$data)
                                }))


if(FALSE){
  library(udpipe)
  library(data.table)
  x <- udpipe("His was talking about marshmallows in New York which was utter bullshit", "english-ewt")
  x <- setDT(x)
  x <- cbind_dependencies(x, type = "parent_rowid")
  x <- cbind_dependencies(x, type = "child_rowid")
  links <- new("syntaxpatterns", data = x, upos %in% "NOUN" & has_child(upos %in% c("ADJ", "ADP")))
  links <- links$run()$sequence()
}