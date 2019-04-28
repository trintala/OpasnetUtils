#' @title case_specifics is a function that replaces default parent information of ovariables using a case-specific table of data.
#' @param specs data.frame containing info about ovariables and the codes of parents. Required column: Name. Optional columns: Ident, Token, Latest, Get, Description.
#' @param verbose logical. If TRUE, explains what is being done. Default: FALSE.
#' @param fetch logical. If TRUE, fecth all parents mentioned in Latest and Get. Default: TRUE.
#' @return function assigns updated values directly to ovariables.

case_specifics <- function(specs, verbose = FALSE, fetch = TRUE) {
  if(fetch) {
    tmp <- setdiff(as.character(specs$Latest), c("",NA))
    if(length(tmp)>0) {
      for(k in tmp) {
        if(verbose) cat(paste('Fetch code', k, '\n', collapse=" "))
        objects.latest(strsplit(k, "\\/")[[1]][1], code_name=strsplit(k, "\\/")[[1]]) # Fetch all the latest codes needed
      }
    }
    
    tmp <- setdiff(as.character(unique(specs$Get)), c("",NA))
    if(length(tmp)>0) {
      for(k in tmp) {
        if(verbose) cat(paste('Fetch code', k, '\n', collapse=" "))
        objects.get(k) # Fetch all the version-specific codes needed
      }
    }
  }
  
  for(i in ls(envir = .GlobalEnv)) {
    if("ovariable" %in% class(get(i))) {
      obj <- get(i)
      depro <- obj@dependencies[obj@dependencies$Name %in% specs$Name , , drop=FALSE]
      if(nrow(depro)>0) {
        if(verbose) {
          cat("Updating ovariable", i, "with original dependencies:\n")
          oprint(obj@dependencies)
        }
        
        tmp <- gsub("^$", NA, as.character(specs$Ident[match(depro$Name, specs$Name)]))
        if(length(tmp)>0 & !all(is.na(tmp))) depro$Ident <- ifelse(is.na(tmp)|tmp=="", as.character(depro$Ident), tmp)
        
        tmp <- gsub("^$", NA, as.character(specs$Token[match(depro$Name, specs$Name)]))
        if(length(tmp)>0 & !all(is.na(tmp))) depro$Token <- ifelse(is.na(tmp)|tmp=="", as.character(depro$Token), tmp)
        
        obj@dependencies <- orbind(
          obj@dependencies[!obj@dependencies$Name %in% specs$Name , , drop=FALSE],
          depro
        )
        assign(i, obj, envir = .GlobalEnv)
        if(verbose) {
          cat("Updated dependencies:\n")
          oprint(get(i)@dependencies)
        }
      }
    }
  }
}
