# paste a string and _file - for creating file names from params
me_File <- function(f) { paste0(f, "_File") }

# paste two strings together - for combining file paths
pathTo <- function(f) { paste0(fileLoc, f) }

# deletes a file
fileInit <- function(f) { if (file.exists(f)) file.remove(f) }

save.as.csv <- function(df, fName) {
  write.csv(df, pathTo(paste0(fName, ".csv")))
} 

# obsolete - before I learned match. Keep for legacy.
# give it a vector and returns a function to search that vector
find_in_vector <- function(vec) { function(find) { any(find == vec) } }

# prediction function for regsubsets objects
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object, id = id)
  mat[,names(coefi)]%*%coefi
}

# metrics from confusion matrix
createMetrics <- function(confmat) {
  metrics <- if (dim(confmat)[1] == 2) {
    ntn <- confmat[1,1]
    nfn <- confmat[1,2]
    ntp <- confmat[2,2]
    nfp <- confmat[2,1]
    list(
      confmat = confmat
      , ntn = ntn
      , nfn = nfn
      , ntp = ntp
      , nfp = nfp
      , accuracy = (ntn+ntp)/sum(confmat)
      , npv = ntn/(ntn+nfn) # accurate negative predictions
      , ppv = ntp/(nfp+ntp) # precision, accurate positive predictions
      , specificity = ntn/(ntn+nfp) # negative cases found
      , sensitivity = ntp/(nfn+ntp) # recall, positive cases found
      , fpr = nfp/(nfp+ntn) # false positive rate, fp out of all negative predictions
    )
  } else {
    if (which(rownames(confmat) == colnames(confmat)) == 1) {
      ntn <- confmat[1,1]
      nfn <- confmat[1,2]
      ntp <- 0
      nfp <- 0
      
      npv <- 1
      ppv <- 0
      specificity <- 1
      sensitivity <- 0
      fpr <- 0
    } else {
      ntn <- 0
      nfn <- 0
      ntp <- confmat[1,2]
      nfp <- confmat[1,1]
      
      npv <- 0
      ppv <- 1
      specificity <- 0
      sensitivity <- 1
      fpr <- 1
    }
    list(
      confmat = confmat
      , ntn = ntn
      , nfn = nfn
      , ntp = ntp
      , nfp = nfp
      , accuracy = accuracy <- confmat[rownames(confmat) == colnames(confmat)]/sum(confmat)
      , npv = npv
      , ppv = ppv
      , specificity = specificity
      , sensitivity = sensitivity
      , fpr = fpr
    )
  }
  return(metrics)
}

# return only the positive part or zero of a numeric vector
pos.part <- function(x) {
  x <- ifelse(x < 0, 0, x)
  return(x)
}

# regression accuracy measures - individual and grouped
MSE <- function(act, pred) {
  sum((pred - act)^2)/length(pred)
} # mean squared error

MAD <- function(act, pred) {
  median(abs(pred - act))
} # absolute median deviation

RMSE <- function(act, pred) {
  sqrt(sum((pred - act)^2)/length(pred))
} # route mean squared error

reg.measures <- function(act, pred) {
  MSE.measure <- MSE(act, pred)
  RMSE.measure <- RMSE(act, pred)
  MAD.measure <- MAD(act, pred)
  data.frame(MSE.measure, RMSE.measure, MAD.measure)
}

# create a formula from column names of a df
generate.full.fmla <- function(resp, dt) {
  nms <- names(dt)
  fmla <- as.formula(paste(resp, "~", paste(nms[!nms %in% resp], collapse = " + ")))
  fmla
}

# create a formula from a vector of column names
generate.fmla <- function(resp, preds) {
  fmla <- as.formula(paste(resp, "~", paste(nms, collapse = " + ")))
}
