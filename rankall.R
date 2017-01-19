rankall <- function(outcome, num = "best"){
     # 1. Read from outcome-of-care-measures.csv file
     data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
     
     # 2. Extract the data of the required 5 columns only: 
     #    hospital(2), state(7), heart attack (11), heart failure (17), pneumonia (23)
     filteredData   <- as.data.frame(cbind(data[, 2], data[, 7],data[, 11], 
                                           data[, 17], data[, 23]), 
                                             stringsAsFactors = FALSE)

     # 3. Define the possible 'Outcomes':
     Outcomes <- c("heart attack", "heart failure", "pneumonia")
     colnames(filteredData) <- c("hospital", "state", Outcomes)

     # 4. check if 'outcome' and 'num' are vaild
     if ((outcome %in% Outcomes) == FALSE) {
          stop(print("-- This is a invalid outcome! --"))
     }
     
     if (is.numeric(num) == FALSE) {
          if (num %in% c("best", "worst") == FALSE) {
               stop(print("-- This is a invalid num! --"))
          }
     }
     
     filteredData[, eval(outcome)] <- as.numeric(filteredData[, eval(outcome)])
     
     if (is.numeric(num)) {
          stateList <- with(filteredData, split(filteredData, state))
          orderedStateList  <- list()
          for (i in seq_along(stateList)){
               stateList[[i]] <- stateList[[i]][order(stateList[[i]][, eval(outcome)], 
                                                    stateList[[i]][, "hospital"]), ]
               orderedStateList[[i]]  <- c(stateList[[i]][num, "hospital"], stateList[[i]][, "state"][1])
          }
          result <- do.call(rbind, orderedStateList)
          output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
          names(output) <- c("hospital", "state")
     } else if (!is.numeric(num)) {
          if (num == "best") {
               stateList <- with(filteredData, split(filteredData, state))
               orderedStateList  <- list()
               for (i in seq_along(stateList)){
                    stateList[[i]] <- stateList[[i]][order(stateList[[i]][, eval(outcome)], 
                                                         stateList[[i]][, "hospital"]), ]
                    orderedStateList[[i]]  <- c(stateList[[i]][1, c("hospital", "state")])
               }
               result <- do.call(rbind, orderedStateList)
               output <- as.data.frame(result, stringsAsFactors = FALSE)
               rownames(output) <- output[, 2]
          } else if (num == "worst") {
               stateList <- with(filteredData, split(filteredData, state))
               orderedStateList  <- list()
               for (i in seq_along(stateList)){
                    stateList[[i]] <- stateList[[i]][order(stateList[[i]][, eval(outcome)], 
                                                         stateList[[i]][, "hospital"], 
                                                         decreasing = TRUE), ]
                    orderedStateList[[i]]  <- c(stateList[[i]][1, c("hospital", "state")])
               }
               result <- do.call(rbind, orderedStateList)
               output <- as.data.frame(result, stringsAsFactors = FALSE)
               rownames(output) <- output[, 2]
          } 
     }
     return(output)
}
