model.cre <- function(object, data, ids, contrasts.arg, method)
{
  method <- if (is.null(method)) "m" else tolower(method)
  attr(object, "intercept") <- 0L
  mat <- model.matrix(object, data, contrasts.arg)[,-1]
  id.table <- table(ids)
  if (all(id.table == id.table[1])) { #balanced
    if (method == "m") {
      # This is much faster than how it is done for unbalanced data...
      cmat <- matrix(rowMeans(matrix(mat, ncol=id.table[1], 
            byrow=TRUE)), ncol=NCOL(mat))[rep(1:length(id.table), 
            each=id.table[1]),,drop=FALSE]
    } else { #ad
      cmat <- matrix(t(mat), ncol=NCOL(mat)*id.table[1], 
            byrow=TRUE)[rep(1:length(id.table), each=id.table[1]),,drop=FALSE]
    }
  } else { #unbalanced
    if (!method %in% c("m", "crem"))
      warning("Panel is unbalanced, defaulting to CRE-M specification")
    cmat <- apply(aggregate(list(mat), by=list(ids), mean)[,-1,drop=FALSE], 
          2, function(z){rep(z, id.table)})
  }

  # Add column names 
  if (method %in% c("m", "crem")) {
    colnames(cmat) <- if (!is.null(colnames(mat)))
      paste("m.", colnames(mat), sep="")
    else
      paste("m.", 1:NCOL(mat),   sep="")
  } else { # ad
   cnam <- if(!is.null(colnames(mat))) colnames(mat) else 1:NCOL(mat)
   colnames(cmat) <- paste("ad", rep(1:id.table[1], each=NCOL(mat)), 
           ".", rep(cnam, id.table[1]), sep="")
  }

  cmat 
}
