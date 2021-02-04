#These two functions calculate various dispersion measures from a document frequency matrix

############
#dispersions_token() calculates the dispersion measures for a single token.
#For example: dispersions_token(my_dfm, "the")
#It returns a named list with all of the dispersion measures discussed by S.T. Gries:
#(http://www.stgries.info/research/dispersion/links.html)

############
#dispersions_all() calculates a subset of of the most common dispersion measures
#for all of the tokens in a document frequncy matrix and returns a dataframe.
#For example: dispersions_all(my_dfm)

dispersions_token <- function(df_matrix, token) {
  
  #select the token counts from the dfm
  #convert to marix
  m <- as.matrix(df_matrix[-1])
  m_norm <- (m/rowSums(m))*1000000
  
  token_idx <- which(colnames(m)==token)
  if (length(token_idx) == 0) stop ("token not found in document feature matrix")
  #convert to a vector
  t_counts <- m[,token_idx]
  t_norm <- m_norm[,token_idx]
  
  #calculate the total number of tokens
  total <- sum(rowSums(m))
  #calculte the relative sizes of the parts of the corpus (in percent)
  parts <- rowSums(m)/total
  #calculate the total number of tokens
  
  token_sd <- sd(t_norm)
  token_cv <- token_sd/mean(t_norm)
  
  # The calc_disp1() function is baesed on a script written by Stefan Th. Gries 
  #It computes commonly used measures of dispersion that he discusses in these papers:
  #Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. /International Journal of Corpus Linguistics/ 13(4). 403-437.
  #Gries, Stefan Th. 2010. Dispersions and adjusted frequencies in corpora: further explorations. In Stefan Th. Gries, Stefanie Wulff, & Mark Davies (eds.), /Corpus linguistic applications: current studies, new directions/, 197-212. Amsterdam: Rodopi.
  
  calc_disp1 <- function(v, s=rep(1/length(v))) {
    
    n <- length(v) # n
    f <- sum(v) # f
    s <- s/sum(s) # s
    
    values <- list()
    
    values[["observed overall frequency"]] <- f
    #note that this is normalizing per million words
    values[["relative frequency"]] <- (f/total)*1000000
    
    values[["range"]] <- round((sum(v>0)/n)*100, 3)
    values[["standard deviation"]] <- round(token_sd, 3)
    values[["variation coefficient CV"]] <- round(token_cv, 3)
    
    values[["deviation of proportions DP"]] <- round(sum(abs((v/f)-s))/2, 3)

    return(values)
  }
  
  #execute the function and return the values
  dsp <- calc_disp1(t_counts, parts)
  dsp <- dplyr::bind_rows(dsp)
  dsp <- t(dsp)
  return(dsp)
}

dispersions_all <- function(df_matrix) {
  
  #convert dfm to matrix object
  m <- as.matrix(df_matrix[,-1])
  #create a column index
  idx <- seq(1:ncol(m))
  
  #create a matrix with normalized frequencies
  m_norm <- (m/rowSums(m))*1000000
  
  token_sd <- matrixStats::colSds(m_norm)
  token_mean <- colMeans(m_norm)
  token_cv <- token_sd/token_mean
  
  
  #calculate the total number of tokens
  total <- sum(rowSums(m))
  #calculte the relative sizes of the parts of the corpus (in percent)
  parts <- rowSums(m)/total
  # The calc_disp() function is baesed on a script written by Stefan Th. Gries 
  #It computes commonly used measures of dispersion that he discusses in these papers:
  #Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. /International Journal of Corpus Linguistics/ 13(4). 403-437.
  #Gries, Stefan Th. 2010. Dispersions and adjusted frequencies in corpora: further explorations. In Stefan Th. Gries, Stefanie Wulff, & Mark Davies (eds.), /Corpus linguistic applications: current studies, new directions/, 197-212. Amsterdam: Rodopi.
  calc_disp2 <- function(v, s=rep(1/length(v))) {
    
    n <- length(v) # n
    f <- sum(v) # f
    s <- s/sum(s) # s
    
    values <- list()
    values[["Absolute_Freq"]] <- f
    #note that this is normalizing per million words
    values[["Relative_Freq"]] <- (f/total)*1000000
    values[["Range"]] <- (sum(ifelse(v > 0, T, F))/n)*100
    
    values[["DP"]] <- sum(abs((v/f)-s))/2
    
    values <- as.data.frame(t(as.matrix(unlist(values))))
    return(values)
  }
  
  #generate a list of dataframes for each token
  dsp <- lapply(idx, function(i){calc_disp2(m[,i], parts)})
  #use the rbindindlist() function to combine into a single dataframe
  dsp <- dplyr::bind_rows(dsp)
  #assign the tokens as row names
  rownames(dsp) <- colnames(m)
  dsp$CV <- token_cv
  dsp <- dsp[order(-dsp$Absolute_Freq),]
  dsp <- cbind(Token = rownames(dsp), data.frame(dsp, row.names=NULL))
  return(dsp)
}
