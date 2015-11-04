# look for any missing NA values before going any further
any(colSums(is.na(submission1)))>0 # result is false so it is safe to do summaries