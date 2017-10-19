
# Two useful Functions
longDat <- function(data, name, num, perturbed = TRUE) {
  if (perturbed == TRUE) {
    if (max(num) > 9) {
  newnames <- paste(name, num, sep="")
  v1s1 <- data[which(data$BATA1==1), c("ID", paste(name,"1A10",1:9, sep=""),
                                       paste(name, "1A1", 10:max(num), sep=""))]
    colnames(v1s1) <- c("id", newnames)
    v1s1$Visit <- 1
    v1s1$Scenario <- 1
  v1s3 <- data[which(data$BATA1==1), c("ID", paste(name, "1A30",1:9, sep=""), paste(name, "1A3", 10:max(num), sep=""))]
    colnames(v1s3)
    colnames(v1s3) <- c("id", newnames)
    v1s3$Visit <- 1
    v1s3$Scenario <- 3
  v1s2 <- data[which(data$BATB1==1), c("ID", paste(name, "1B20",1:9, sep=""), paste(name, "1B2", 10:max(num), sep=""))]
    colnames(v1s2)
    colnames(v1s2) <- c("id", newnames)
    v1s2$Visit <- 1
    v1s2$Scenario <- 2
  v1s4 <- data[which(data$BATB1==1), c("ID", paste(name, "1B40",1:9, sep=""), paste(name, "1B4", 10:max(num), sep=""))]
    colnames(v1s4)
    colnames(v1s4) <- c("id", newnames)
    v1s4$Visit <- 1
    v1s4$Scenario <- 4
  v2s1 <- data[which(data$BATA2==1), c("ID", paste(name, "2A10",1:9, sep=""), paste(name, "2A1", 10:max(num), sep=""))]
    colnames(v2s1)
    colnames(v2s1) <- c("id", newnames)
    v2s1$Visit <- 2
    v2s1$Scenario <- 1
  v2s3 <- data[which(data$BATA2==1), c("ID", paste(name, "2A30",1:9, sep=""), paste(name, "2A3", 10:max(num), sep=""))]
    colnames(v2s3)
    colnames(v2s3) <- c("id", newnames)
    v2s3$Visit <- 2
    v2s3$Scenario <- 3
  v2s2 <- data[which(data$BATB2==1), c("ID", paste(name, "2B20",1:9, sep=""), paste(name, "2B2", 10:max(num), sep=""))]
    colnames(v2s2)
    colnames(v2s2) <- c("id", newnames)
    v2s2$Visit <- 2
    v2s2$Scenario <- 2
  v2s4 <- data[which(data$BATB2==1), c("ID", paste(name, "2B40",1:9, sep=""), paste(name, "2B4", 10:max(num), sep=""))]
    colnames(v2s4)
    colnames(v2s4) <- c("id", newnames)
    v2s4$Visit <- 2
    v2s4$Scenario <- 4

  #Combining all together into a new set
  longdata <- rbind(v1s1, v1s2, v1s3, v1s4, v2s1, v2s2, v2s3, v2s4)
  #allid's with visit for each
  longid1 <- data[, c("id", "BAT1", "BAT2")]
  longid1$Visit <- 1
  longid2 <- data[, c("id", "BAT1", "BAT2")]
  longid2$Visit <- 2
  longid <- rbind(longid1, longid2)
  #merge together, output should have 854*2 rows
  longdata <- merge(longdata, longid, all.y=TRUE)

  #insert some contrast codes
  # S1 vs S2
  longdata[which(longdata[,"Scenario"]==1), "ec2"] <- 0.5
  longdata[which(longdata[,"Scenario"]==2), "ec2"] <- -0.5
  longdata[which(longdata[,"Scenario"]==3), "ec2"] <- 0
  longdata[which(longdata[,"Scenario"]==4), "ec2"] <- 0
  # S1-2 vs S3
  longdata[which(longdata[,"Scenario"]==1), "ec3"] <- 0.5
  longdata[which(longdata[,"Scenario"]==2), "ec3"] <- 0.5
  longdata[which(longdata[,"Scenario"]==3), "ec3"] <- -1
  longdata[which(longdata[,"Scenario"]==4), "ec3"] <- 0
  #S1-3 vs S4
  longdata[which(longdata[,"Scenario"]==1), "ec4"] <- .3333
  longdata[which(longdata[,"Scenario"]==2), "ec4"] <- .3333
  longdata[which(longdata[,"Scenario"]==3), "ec4"] <- .3333
  longdata[which(longdata[,"Scenario"]==4), "ec4"] <- -1

  #reorder to make it clearer
  longdata <- longdata[,c("id","BAT1", "BAT2",
                          "Visit", "Scenario", "ec2", "ec3", "ec4",
                          newnames)]
  return(longdata)
    }
    else {
  newnames <- paste(name, num, sep="")
  v1s1 <- data[which(data$BATA1==1), c("ID", paste(name,"1A10",num, sep=""))]
    colnames(v1s1) <- c("id", newnames)
    v1s1$Visit <- 1
    v1s1$Scenario <- 1
  v1s3 <- data[which(data$BATA1==1), c("ID", paste(name, "1A30",num, sep=""))]
    colnames(v1s3)
    colnames(v1s3) <- c("id", newnames)
    v1s3$Visit <- 1
    v1s3$Scenario <- 3
  v1s2 <- data[which(data$BATB1==1), c("ID", paste(name, "1B20",num, sep=""))]
    colnames(v1s2)
    colnames(v1s2) <- c("id", newnames)
    v1s2$Visit <- 1
    v1s2$Scenario <- 2
  v1s4 <- data[which(data$BATB1==1), c("ID", paste(name, "1B40",num, sep=""))]
    colnames(v1s4)
    colnames(v1s4) <- c("id", newnames)
    v1s4$Visit <- 1
    v1s4$Scenario <- 4
  v2s1 <- data[which(data$BATA2==1), c("ID", paste(name, "2A10",num, sep=""))]
    colnames(v2s1)
    colnames(v2s1) <- c("id", newnames)
    v2s1$Visit <- 2
    v2s1$Scenario <- 1
  v2s3 <- data[which(data$BATA2==1), c("ID", paste(name, "2A30",num, sep=""))]
    colnames(v2s3)
    colnames(v2s3) <- c("id", newnames)
    v2s3$Visit <- 2
    v2s3$Scenario <- 3
  v2s2 <- data[which(data$BATB2==1), c("ID", paste(name, "2B20",num, sep=""))]
    colnames(v2s2)
    colnames(v2s2) <- c("id", newnames)
    v2s2$Visit <- 2
    v2s2$Scenario <- 2
  v2s4 <- data[which(data$BATB2==1), c("ID", paste(name, "2B40",num, sep=""))]
    colnames(v2s4)
    colnames(v2s4) <- c("id", newnames)
    v2s4$Visit <- 2
    v2s4$Scenario <- 4

  #Combining all together into a new set
  longdata <- rbind(v1s1, v1s2, v1s3, v1s4, v2s1, v2s2, v2s3, v2s4)
  #allid's with visit for each
  longid1 <- data[, c("id", "BAT1", "BAT2")]
  longid1$Visit <- 1
  longid2 <- data[, c("id", "BAT1", "BAT2")]
  longid2$Visit <- 2
  longid <- rbind(longid1, longid2)
  #merge together, output should have 854*2 rows
  longdata <- merge(longdata, longid, all.y=TRUE)

  #insert some contrast codes
  # S1 vs S2
  longdata[which(longdata[,"Scenario"]==1), "ec2"] <- 0.5
  longdata[which(longdata[,"Scenario"]==2), "ec2"] <- -0.5
  longdata[which(longdata[,"Scenario"]==3), "ec2"] <- 0
  longdata[which(longdata[,"Scenario"]==4), "ec2"] <- 0
  # S1-2 vs S3
  longdata[which(longdata[,"Scenario"]==1), "ec3"] <- 0.5
  longdata[which(longdata[,"Scenario"]==2), "ec3"] <- 0.5
  longdata[which(longdata[,"Scenario"]==3), "ec3"] <- -1
  longdata[which(longdata[,"Scenario"]==4), "ec3"] <- 0
  #S1-3 vs S4
  longdata[which(longdata[,"Scenario"]==1), "ec4"] <- .3333
  longdata[which(longdata[,"Scenario"]==2), "ec4"] <- .3333
  longdata[which(longdata[,"Scenario"]==3), "ec4"] <- .3333
  longdata[which(longdata[,"Scenario"]==4), "ec4"] <- -1

  #reorder to make it clearer
  longdata <- longdata[,c("id","BAT1", "BAT2",
                          "Visit", "Scenario", "ec2", "ec3", "ec4",
                          newnames)]
  return(longdata)
    }
    }

  else {
    if (max(num) > 9) {
  newnames <- paste(name, num, sep="")
  v1a <- data[which(data$BAT1==1), c("ID", paste(name,"1A00",1:9, sep=""),
                                       paste(name, "1A0", 10:max(num), sep=""))]
    colnames(v1a) <- c("id", newnames)
    v1a$Visit <- 1
  v1b <- data[which(data$BAT1==2), c("ID", paste(name, "1B00",1:9, sep=""), paste(name, "1B0", 10:max(num), sep=""))]
    colnames(v1b) <- c("id", newnames)
    v1b$Visit <- 1
  v2a <- data[which(data$BAT2==1), c("ID", paste(name, "2A00",1:9, sep=""), paste(name, "2A0", 10:max(num), sep=""))]
    colnames(v2a) <- c("id", newnames)
    v2a$Visit <- 2
  v2b <- data[which(data$BAT2==2), c("ID", paste(name, "2B00",1:9, sep=""), paste(name, "2B0", 10:max(num), sep=""))]
    colnames(v2b) <- c("id", newnames)
    v2b$Visit <- 2


  #Combining all together into a new set
  longdata <- rbind(v1a, v1b, v2a, v2b)
  #allid's with visit for each
  longid1 <- data[, c("id", "BAT1", "BAT2")]
  longid1$Visit <- 1
  longid2 <- data[, c("id", "BAT1", "BAT2")]
  longid2$Visit <- 2
  longid <- rbind(longid1, longid2)
  #merge together, output should have 854*2 rows
  longdata <- merge(longdata, longid, all.y=TRUE)

  #reorder to make it clearer
  longdata <- longdata[,c("id","BAT1", "BAT2",
                          "Visit", newnames)]
  return(longdata)
    }
    else {
        newnames <- paste(name, num, sep="")
  v1a <- data[which(data$BAT1==1), c("ID", paste(name,"1A00",num, sep=""))]
    colnames(v1a) <- c("id", newnames)
    v1a$Visit <- 1
  v1b <- data[which(data$BAT1==2), c("ID", paste(name, "1B00",num, sep=""))]
    colnames(v1b) <- c("id", newnames)
    v1b$Visit <- 1
  v2a <- data[which(data$BAT2==1), c("ID", paste(name, "2A00",num, sep=""))]
    colnames(v2a) <- c("id", newnames)
    v2a$Visit <- 2
  v2b <- data[which(data$BAT2==2), c("ID", paste(name, "2B00",num, sep=""))]
    colnames(v2b) <- c("id", newnames)
    v2b$Visit <- 2


  #Combining all together into a new set
  longdata <- rbind(v1a, v1b, v2a, v2b)
  #allid's with visit for each
  longid1 <- data[, c("id", "BAT1", "BAT2")]
  longid1$Visit <- 1
  longid2 <- data[, c("id", "BAT1", "BAT2")]
  longid2$Visit <- 2
  longid <- rbind(longid1, longid2)
  #merge together, output should have 854*2 rows
  longdata <- merge(longdata, longid, all.y=TRUE)

  #reorder to make it clearer
  longdata <- longdata[,c("id","BAT1", "BAT2",
                          "Visit", newnames)]
  return(longdata)

    }
}}
# This function should be useful
harmonize <- function(data, varnames, scenario, oldvalue, newvalue) {
  for(i in seq(varnames)) {
    data[which(data[, "Scenario"]==scenario&data[,varnames[i]]==oldvalue),
         varnames[i]] <- newvalue
  }
  return(data[,varnames])
}



TestDifModels <- function(directory = getwd(),
                          nameOfImpactModel,
                          namesofDifModels,
                          allMplusSummaries = NULL) {
  # To do
  # 2. option to feed in MPLUS extracted models and not read everytime
  # make all names lowercase
  nameOfImpactModel <- tolower(nameOfImpactModel)
  namesofDifModels  <- tolower(namesofDifModels)
  # if user provodes the dataframe of all models use that
  if (is.null(allMplusSummaries)) {
    allMplusSummaries <- MplusAutomation::extractModelSummaries(target = directory)
  }
  # Save the Comparison Model
  refModel <- allMplusSummaries[grep(nameOfImpactModel,
                                     allMplusSummaries$Filename),           # Select Rows
                                c("Filename", "Parameters", "LL", "BIC")]   # Select Cols
  # Save the other models in a dataframe
  compModels <- data.frame()
  for (i in seq(namesofDifModels)) {
    temp <- allMplusSummaries[grep(namesofDifModels[i],
                                   allMplusSummaries$Filename),             # Select Rows
                              c("Filename", "Parameters", "LL", "BIC")]          # Select Cols
    compModels <- rbind(compModels, temp)
  }
  # Compute a LRT's ect
  compModels$LRT    <- 2*(compModels$LL-refModel$LL)
  compModels$LRT.df <- compModels$Parameters - refModel$Parameters
  compModels$LRT.p  <- pchisq(compModels$LRT, df = compModels$LRT.df, lower.tail = FALSE)
  # Sort for most Significant
  compModels <- compModels[order(compModels$LRT.p, decreasing = FALSE),]
  # Return the dataFrame
  refModel$LRT    <- NA
  refModel$LRT.df <- NA
  refModel$LRT.p  <- NA
  return(rbind(refModel, compModels))
}

MplusErrors <- function(directory = getwd(), filePattern = NULL) {
  # TODO
  # Think about ways to make this function quicker
  # Check MplusAutomation for how it reads files
  # Could check performance with read_lines() from the readr package vs base readLines().

  # First create the pattern to identify files
  if (is.null(filePattern)) {
    myPattern = ".out"
  } else {
    myPattern = paste(filePattern, ".*.out", sep = "")
  }
  # Add slashes to end of directory
  if (substr(directory, start = nchar(directory),stop = nchar(directory)) == "\\" |
      substr(directory, start = nchar(directory),stop = nchar(directory)) == "/") {
  } else {
    directory <- paste(directory, "\\", sep = "")
  }
  # create a list and start save file names
  results <- NULL
  results$names = list.files(path = directory,
                             pattern = myPattern,
                             all.files = FALSE,
                             full.names = ,
                             recursive = FALSE,
                             ignore.case = TRUE,
                             include.dirs = FALSE,
                             no.. = FALSE)
  # Go over every file read it and search it
  for (i in seq(results$names)) {
    #temp                          <- readLines(paste(directory, results$names[i], sep = ""))
    temp                          <- readr::read_lines(file = paste(directory, results$names[i], sep = ""))
    results$NONnorm_terminate[i]           <- any(grepl(x=temp, pattern = "MODEL ESTIMATION DID NOT TERMINATE NORMALLY", ignore.case = TRUE))
    results$saddle[i]             <- any(grepl(x=temp, pattern = "saddle", ignore.case = TRUE))
    results$avoidSingularity[i]   <- any(grepl(x=temp, pattern = "avoid singularity", ignore.case = TRUE))

  }
  readr::read_lines(file = paste(directory, results$names[i], sep = ""))
  # create a DF of results
  # list to DF
  df <- do.call(cbind.data.frame, results)
  # list to DF with unlist
  # df <- data.frame(matrix(unlist(results), nrow =  length(results$names), byrow=F))
  # list to DF with plyr - doesn't work
  # df <- plyr::ldply (results, data.frame)
  return(df)
}


MplusElapsedTime <- function(pathToFile) {
  # TODO
  if (substr(pathToFile,
             start = nchar(pathToFile)-3,
             stop  = nchar(pathToFile)) != ".out") {
    stop("File needs to be an Mplus '.out' file")
  }
  # Check that the file is a .out file
  # Read in file
  tmp <- readLines(pathToFile)
  tmp <- tmp[length(tmp)-13]
  seconds <- as.integer(substr(tmp,  start = nchar(tmp)-1, stop = nchar(tmp)))
  minutes <- as.integer(substr(tmp,  start = nchar(tmp)-4, stop = nchar(tmp)-3))
  totalsec <- (minutes*60)+seconds
  totalmin <- round(totalsec/60, digits = 1)
  # Subset the elapsed time
  #return(paste(pathToFile, ": ", minutes, "min ", seconds, "s"), sep = "")
  #return(paste(pathToFile, ": ", totalsec, " seconds", sep = ""))
  return(totalmin)
}

# adding some work from the UNC computer
