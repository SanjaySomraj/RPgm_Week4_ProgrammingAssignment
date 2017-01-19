rankhospital <- function(state, outcome, num = "best"){
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

     # 6. Assign ranks based input 'num'
     #    if input num is 'best' then then num = 1
     #    if input num is 'worst' then num = last
     #    if input num is a number then use num as is
     if(num == "best") {
          num = 1
          output <- best(state, outcome)
     }
     else if(num == "worst") {
          stateData <- which(filteredData[, "state"] == state)
          filteredStateData <- filteredData[stateData,]    
          filteredStateData[, eval(outcome)] <- suppressWarnings(as.numeric(filteredStateData[, eval(outcome)]))
          filteredStateData <- filteredStateData[order(filteredStateData[, eval(outcome)], filteredStateData[, "hospital"], decreasing = TRUE), ]
          output <- filteredStateData[, "hospital"][1]
     }
     else if (is.numeric(num)){
          stateData <- which(filteredData[, "state"] == state)
          filteredStateData <- filteredData[stateData,]
          filteredStateData[, eval(outcome)] <- suppressWarnings(as.numeric(filteredStateData[, eval(outcome)]))
          filteredStateData <- filteredStateData[order(filteredStateData[, eval(outcome)], filteredStateData[, "hospital"]), ]
          output <- filteredStateData[, "hospital"][num]
     }
     else {
          stop(print("-- This is a invalid num! --"))
     }
     
     return(output)
}
