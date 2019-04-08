getDatasetList <- function() {
  
  data_list <- data.frame(data()[3])
  data_list <- do.call(rbind, strsplit(as.character(data_list$results.Item), " "))[,1]
  
  return(data_list)
  
}

getColumnsType <- function(data) {
  
  #Get datatype of each column
  header <- names(data)
  
  numeric_list <- NULL
  non_numeric_list <- NULL
  others_list <- NULL
  
  for(col in header) {
    
    ifelse(is.ts(data[, col]), others_list <- c(others_list, col),
           ifelse(is.factor(data[, col]), non_numeric_list <- c(non_numeric_list, col),
                  ifelse(is.numeric(data[, col]), numeric_list <- c(numeric_list, col), non_numeric_list <- c(non_numeric_list, col))
           )
    )
    
  }
  
  return(list(numeric_list, non_numeric_list, others_list))
  
}
##############################################################################################
#Preprocess of clustering

dataPreprocess_Clustering <- function(data) {
  
  col_types <- getColumnsType(data)
  numeric_list <- unlist(col_types[1])
  non_numeric_list <- unlist(col_types[2])
  others_list <- unlist(col_types[3])
  
  for(col in non_numeric_list) {
    data[, col] <- NULL
  }
  
  for(col in others_list) {
    data[, col] <- NULL
  }
  
  data <- data[complete.cases(data),]
  raw_data <- data
  data <- scale(data)
  
  return(list(raw_data, data))
  
}

##############################################################################################
#Preprocess of classification

dataPreprocess_Classification <- function(data) {
  
  col_types <- getColumnsType(data)
  numeric_list <- unlist(col_types[1])
  non_numeric_list <- unlist(col_types[2])
  others_list <- unlist(col_types[3])
  
  data <- data[complete.cases(data),]
  
  sample_number <- sample(1:nrow(data), nrow(data)/3*2, replace=FALSE)
  training_dataset <- data[sample_number, ]
  testing_dataset <- data[-sample_number, ]
  
  return(list(training_dataset, testing_dataset))
  
}

##############################################################################################
dataSummary_numreic <- function(data) {
  
  if(is.data.frame(data)) {
    
    #Get datatype of each column
    col_types <- getColumnsType(data)
    numeric_list <- unlist(col_types[1])
    
    #Numeric
    
    mean_list <- NULL
    sd_list <- NULL
    min_list <- NULL
    Q1_list <- NULL
    median_list <- NULL
    Q3_list <- NULL
    max_list <- NULL
    IQR_list <- NULL
    N_list <- NULL
    
    for(col in numeric_list) {
      
      mean_list <- c(mean_list, mean(data[, col], na.rm = TRUE))
      sd_list <- c(sd_list, sd(data[, col], na.rm = TRUE))
      
      quantile <- quantile(data[, col], na.rm = TRUE)
      
      min_list <- c(min_list, quantile[1])
      Q1_list <- c(Q1_list, quantile[2])
      median_list <- c(median_list, quantile[3])
      Q3_list <- c(Q3_list, quantile[4])
      max_list <- c(max_list, quantile[5])
      
      IQR_list <- c(IQR_list, IQR(data[, col], na.rm = TRUE))
      N_list <- c(N_list, length(data[, col]) - sum(is.na(data[, col])))
      
    }
    
    numeric_summary <- data.frame(numeric_list, mean_list, sd_list, min_list, Q1_list, median_list, Q3_list, max_list, IQR_list, N_list)
    names(numeric_summary) <- c("Attribute", "Mean", "Sd", "Min", "25%", "Median", "75%", "Max", "IQR", "N")
    
    correlation_matrix <- cor(data[, numeric_list], use="complete")
    
    
    return(list(numeric_summary, correlation_matrix))
    
  }else {
    
    return(NULL)
    
  }
  
}

##############################################################################################

dataSummary_non_numeric <- function(data) {
  
  if(is.data.frame(data)) {
    
    #Get datatype of each column
    col_types <- getColumnsType(data)
    non_numeric_list <- unlist(col_types[2])
    
    non_numeric_summary <- NULL
    
    for(col in non_numeric_list) {
      
      temp <- data.frame(table(data[, col]))
      names(temp) <- c(col, "Freq")
      non_numeric_summary <- if(is.null(non_numeric_summary)) list(temp) else list(non_numeric_summary, list(temp))
      
    }
    
    return(non_numeric_summary)
    
  }else {
    
    return(NULL)
    
  }
  
}

