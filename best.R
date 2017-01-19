best <- function(state, outcome) {
     
     # 1. Read from outcome-of-care-measures.csv file
     data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
     
     # 2. Extract the data of the required 5 columns only: 
     #    hospital(2), state(7), heart attack (11), heart failure (17), pneumonia (23)
     filteredData   <- as.data.frame(cbind(data[, 2], data[, 7],data[, 11], 
                                           data[, 17], data[, 23]), 
                                     stringsAsFactors = FALSE)
     
     # 3. Exrtact distinct 'States' from the CSV data
     States <- levels(factor(data[, 7]))
     
     # 4. Define the possible 'Outcomes':
     Outcomes <- c("heart attack", "heart failure", "pneumonia")
     colnames(filteredData) <- c("hospital", "state", Outcomes)
     
     # 5. check if 'state' and 'outcome' are vaild
     if ((state %in% States) == FALSE) {
          stop(print("-- This is a invalid state! --"))
     }

     if ((outcome %in% Outcomes) == FALSE) {
          stop(print("-- This is a invalid outcome! --"))
     }
     
     # 6. Define mapping of outcome column according to 'outcome' input
     stateData <- which(filteredData[, "state"] == state)
     filteredStateData <- filteredData[stateData, ]    # extracting data for the called state
     filteredStateOutcome <- suppressWarnings(as.numeric(filteredStateData[, eval(outcome)]))
     min_val <- min(filteredStateOutcome, na.rm = TRUE)
     result  <- filteredStateData[, "hospital"][which(filteredStateOutcome == min_val)]
     output  <- result[order(result)]
     
     return(output)
}
