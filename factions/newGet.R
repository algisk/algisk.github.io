library(data.table)
library(foreach)
library(rjson)

# Functions
getResp <- function(call) {
    tryCatch({
        resp <- fromJSON(file=call)
    },
    error=function(cond) {
        message("ERROR!")
        message(cond)
        resp <- list("error"="Failed to read URL!")
        return(resp)
    },
    warning=function(cond) {
        message("WARNING!")
        message(cond)
        resp <- list("error"="Failed to read URL!")
        return(resp)
    },
    finally={
        message("")
    })
}
getStats <- function(call, api=API) {
    counter <- 1
    fullCall <- paste0(call,sample(api,1))
    resp <- getResp(fullCall)
    # resp <- fromJSON(file=call)
    while(names(resp)[1]=="error") {
        if(resp$error$code==6) {
            return(resp)
        } else {
            print(resp$error)
            Sys.sleep(0.1)
            fullCall <- paste0(call,sample(api,1))
            resp <- getResp(fullCall)
            counter <- counter+1
            if(counter>=50){
                break
            }
        }
    }
    return(resp)
}

# Setup
API <- c("GaEoWrd6xXsm25vm","PpifTjplUkMKPeQr","kdjFAlXkaLjRf75v","LQEHPez1AgyGlMD3","5Jq2etHJ6oTqqHBD","aY7nKoMQsMjOKxRU")
# Go through all possible IDs
facList <- data.table()
facMembs <- list()
goodFacs <- c()
facOut <- foreach(I=1:60000) %do% {
    fcall <- paste0("https://api.torn.com/faction/",I,"?selections=basic&key=")
    finit <- getStats(fcall,API)
    if(!(names(finit)[1]=="error")) {
        goodFacs <- c(goodFacs,I)
        tmp <- data.table(ID=finit$ID, tag_image=finit$tag_image, respect=finit$respect, 
        name=finit$name, leader=finit$leader, age=finit$age, tag=finit$tag, co_leader=finit$`co-leader`, 
        capacity=finit$capacity, members=length(finit$members), rank=finit$rank$name, division=finit$rank$division, 
        level=finit$rank$level, position=finit$rank$position, wins=finit$rank$wins)
        tmp$leader_name <- finit$members[[as.character(tmp$leader)]]$name
        tmp$co_leader_name <- ifelse(tmp$co_leader==0,NA,finit$members[[as.character(tmp$co_leader)]]$name)
        tmp$leader_last_action <- finit$members[[as.character(tmp$leader)]]$last_action$timestamp
        facMembs[[I]] <- finit$members
        facList <- rbind(facList, tmp, fill=TRUE)
        print(paste0(I,": ",finit$name))
        Sys.sleep(0.1)
        tmp
    } else {
        print(paste0(I,": ID not found"))
        Sys.sleep(0.1)
    }
}

writeLines(as.character(goodFacs),"goodFacs.txt")
write.csv(facList,"facList.csv", row.names=FALSE)
saveRDS(facMembs, "facMembs.RDS")
