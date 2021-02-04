
dispersions_all <- function(df_matrix) {
  
  if (class(df_matrix) != "dfm") stop ("your target must be a quanteda dfm object")
  #convert dfm to matrix object
  m <- as.matrix(df_matrix)
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
