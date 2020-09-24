if(FALSE){
  library(data.table)
  library(methods)
}

# expr_list(a = 1, b = 10, 100)
expr_list <- function(...){
  #expr <- substitute(list(...))
  #expr <- as.list(expr[-1])
  expr <- substitute(...())  
  expr
}

# x <- expr_list(a = 1, b = 10, 100)
# expr_labels(x)
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



# x <- c(4, 3, 2, 20)
# index_to_index(x)
index_to_index <- function(x){
  x <- unclass(x)
  DT <- mapply(seq_along(x), x, FUN=function(i, id){
    if(length(id) > 0){
      list(row = i, row_childparent = id)
    }else{
      list(row = integer(0), row_childparent = integer(0))
    }
  }, SIMPLIFY = FALSE)
  DT <- data.table::rbindlist(DT)
  DT
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
    index_to_index(x)
  }))




##
## linkChain class indicating the chain of child/parent links
##
linkChain <- setRefClass("linkChain", fields = list(rowids = "rowLinks", 
                                                    data = "data.table", 
                                                    n = "integer", 
                                                    current_depth = "integer",
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
    index_to_index(x)
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
    .self$data  <- merge(links, .self$data, by.x = "link", by.y = "link", all.x = FALSE, all.y = FALSE, allow.cartesian = TRUE,
                         suffixes = c("newlink", paste("_depth", depth, sep = "")))
    .self$data <- data.table::setnames(.self$data, old = "link", new = label)
    ## Maintain some administration
    .self$last_matched_on <- label
    .self$labels <- c(.self$labels, label)
    .self$current_depth <- as.integer(depth)
    .self$data <- data.table::setcolorder(.self$data, neworder = c("row", .self$labels, "row_childparent"))
    invisible(.self)
  },
  join = function(chain){
    stopifnot(inherits(chain, "linkChain"))
    .self$current_depth <- .self$current_depth + chain$current_depth + 1L
    depth <- .self$current_depth
    newname <- paste("row_link_", depth, sep = "")
    
    links       <- data.table::setnames(chain$data, old = "row", new = "link")
    .self$data  <- data.table::setnames(.self$data, old = "row_childparent", new = "link")
    if(length(.self$labels) > 0){
      ## rename to avoid conflicts
      newlabels <- paste("row_link_", seq.int(from = chain$data$current_depth + 1, 
                                              to   = chain$data$current_depth + seq_along(.self$labels)), sep = "")
      .self$data   <- data.table::setnames(.self$data, old = .self$labels, new = newlabels)
      .self$labels <- newlabels
    }
    .self$data  <- merge(chain$data, .self$data, by.x = "link", by.y = "link", all.x = FALSE, all.y = FALSE, allow.cartesian = TRUE,
                         suffixes = c("newlink", paste("_depth", depth, sep = "")))
    .self$data <- data.table::setnames(.self$data, old = "link", new = newname)
    ## Maintain some administration
    .self$last_matched_on <- newname
    .self$labels <- c(.self$labels, newname)
    .self$current_depth <- as.integer(depth)
    .self$data <- data.table::setcolorder(.self$data, neworder = c("row", .self$labels, "row_childparent"))
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
    
    ## combine existing set of links in .self$data with the OR links
    ## .self$data can have columns like row/row_link_1/row_link_2/row_childparent where row is the start and row_link_{NR} is intermediate ones
    #browser("Need to debug OR")
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
    only <- union(only, .self$labels)
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





#' @title Experimental and undocumented querying of syntax relationships 
#' @description Currently undocumented
#' @export 
#' @rdname syntaxrelation
#' @param e1 Currently undocumented 
#' @param e2 Currently undocumented
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
    contains_syntaxrelation <- sapply(expressions, FUN = function(e) 
      any(c("relationship", "has_child", "has_parent") %in% all.names(e)), USE.NAMES = FALSE)
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
    #cat("START>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", sep = "\n")
    #print(expression)
    #print(class(DT))
    label       <- .self$expr$expr[[1]]
    result      <- eval(expression, DT)
    #cat("DONE>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", sep = "\n")
    #print(expression)
    #print(class(result))
    #print(.self$type)
    if(is.logical(result)){
      ## FILTER OUTPUT
      #print(expression)
      #print(.self$type)
      # TODO same point here, in case of default, 
      # we need to flag somehow that the 2nd level is not usefull yet in order not to display it when using sequence
      if(.self$type == "default"){
        result <- chain$filter(result, type = "row")  
      }else if(.self$type %in% c("child", "parent")){
        result <- chain$filter(result, type = "row_childparent")  
      }
      
    }else if(inherits(result, "linkChain")){
      ## CHILD/PARENT LOGIC
      if(.self$type %in% c("child", "parent")){
        result <- chain$join(result)
      }
    }else if(inherits(result, "syntaxrelation")){
      ## GO DEEPER
      result <- result$run(DT = DT)
    }
    result
  }))


#' @rdname syntaxrelation
#' @aliases |,syntaxrelation,logical-method
setMethod("|", signature = c("syntaxrelation", "logical"),
          definition = function(e1, e2) {
            chain <- e1$run(parent.frame())
            chain$OR(e2)
            .NotYetImplemented() ## Give error as long as issue with labels is not solved yet
          })

#' @rdname syntaxrelation
#' @aliases |,logical,syntaxrelation-method
setMethod("|", signature = c("logical", "syntaxrelation"),
          definition = function(e1, e2) {
            chain <- e2$run(parent.frame())
            chain$OR(e1)
            .NotYetImplemented() ## Give error as long as issue with labels is not solved yet
          })

#' @rdname syntaxrelation
#' @aliases &,syntaxrelation,logical-method
setMethod("&", signature = c("syntaxrelation", "logical"),
          definition = function(e1, e2) {
            chain <- e1$run(parent.frame())
            chain$AND(e2)
          })

#' @rdname syntaxrelation
#' @aliases &,logical,syntaxrelation-method
setMethod("&", signature = c("logical", "syntaxrelation"),
          definition = function(e1, e2) {
            chain <- e2$run(parent.frame())
            chain$AND(e1)
          })

#' @title Experimental and undocumented querying of syntax patterns 
#' @description Currently undocumented
#' @export 
#' @rdname syntaxpatterns
syntaxpatterns <- setRefClass("syntaxpatterns",
                              fields = list(data = "data.table", relations = "syntaxrelation"),
                              methods = list(
                                initialize = function(data, ...) {
                                  .self$data        <- data
                                  .self$relations   <- new("syntaxrelation", ...)
                                  if(!.self$relations$has_child_parent_logic()){
                                    #stop("This does not make sense, there is no call to has_child or has_parent in your code")
                                  }
                                  invisible(.self)
                                },
                                run = function(){
                                  .self$relations$run(.self$data)
                                }))


if(FALSE){
  #library(udpipe)
  library(data.table)
  x <- udpipe::udpipe("His was talking about marshmallows in New York which was utter bullshit", "english-ewt")
  x <- setDT(x)
  x <- udpipe::cbind_dependencies(x, type = "parent_rowid")
  x <- udpipe::cbind_dependencies(x, type = "child_rowid")
  #x <- udpipe::cbind_dependencies(x, type = "parent_rowid", recursive = TRUE)
  #x <- udpipe::cbind_dependencies(x, type = "child_rowid", recursive = TRUE)
  saveRDS(x, file = "x.rds")
  library(data.table)
  x <- readRDS("x.rds")
  
  ##
  ## HIGH LEVEL CALLS
  ##
  links <- new("syntaxpatterns", data = x, upos %in% "NOUN" & has_child(upos %in% c("ADJ", "ADP")))
  links <- links$run()$sequence()
  
  cp <- new("syntaxpatterns", data = x, upos %in% "NOUN")
  links <- cp$run()$sequence()
  cp <- new("syntaxpatterns", data = x, has_child(upos %in% "ADJ"))
  cp <- new("syntaxpatterns", data = x, upos %in% "PROPN" | has_child(upos %in% c("ADJ", "ADP")))
  cp <- new("syntaxpatterns", data = x, upos %in% "NOUN" | has_child(upos %in% c("ADJ", "ADP")))
  cp <- new("syntaxpatterns", data = x, upos %in% "NOUN" & has_child(upos %in% c("ADJ", "ADP")))
  cp <- new("syntaxpatterns", data = x, upos %in% "VERB" & has_child(adj = upos %in% c("NOUN", "AUX")))
  cp <- new("syntaxpatterns", data = x, upos %in% "VERB" & has_child(upos %in% c("NOUN", "AUX") & has_child(!is.na(upos))))
  cp <- new("syntaxpatterns", data = x, has_child(!is.na(upos) & has_child(grandchild = upos %in% "NOUN")))
  cp <- new("syntaxpatterns", data = x, has_child(upos %in% "NOUN" & has_child(grandchild = upos %in% "NOUN")))
  cp <- new("syntaxpatterns", data = x, has_child(upos %in% "NOUN" & has_child(grandchild = upos %in% "NOUN" & has_parent(!is.na(upos)))))
  ok <- cp$run()
  str(ok$sequence())
  str(ok)
  
  ##
  ## LOW LEVEL CALLS
  ##
  chain <- new("linkChain", rowids = new("rowLinks", x$child_rowid))
  all.equal(unclass(chain$links()), unclass(x$child_rowid))
  str(chain$links())
  str(x$child_rowid)
  chain$data
  
  
  # this is: has_child(dep_rel %in% "acl:relcl")
  chain <- new("linkChain", rowids = new("rowLinks", x$child_rowid))
  links <- list(integer(0), integer(0), integer(0), integer(0), c(12L), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0))
  chain$match(links)
  chain$data
  str(chain$links())
  chain <- new("linkChain", rowids = new("rowLinks", x$child_rowid))
  which(x$upos %in% "NOUN")
  chain$AND(x$upos %in% "NOUN")
  chain$data
  str(chain$links())
  
  chain <- new("linkChain", rowids = new("rowLinks", x$child_rowid))
  chain$filter(seq_len(chain$n) %in% c(1:2), type = "row_childparent")
  chain$data
  chain$filter(x$upos %in% "NOUN")
  chain$links()
  
  chain <- new("linkChain", rowids = new("rowLinks", x$child_rowid))
  links <- list(integer(0), integer(0), integer(0), integer(0), c(12L, 11L), integer(0), integer(0),  integer(0), integer(0), integer(0), integer(0), integer(0))
  chain$data
  chain$match(links)
  chain$links()
  chain$sequence()
  links <- list(integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0),  integer(0), integer(0), integer(0), integer(0), c(11L))
  chain$match(links)
  chain$sequence()
}