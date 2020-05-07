############################################
##### Defining rules into functions
############################################


##########################
## Installing libraries
##########################

library("haven")


library(dplyr)



###########################
## Reaing datasets
###########################

dm <- read_xpt("C:\\Users\\MAHESAR1\\Desktop\\DEDRR\\P21\\P21 rules\\datasets\\dm.xpt")

ex <- read_xpt("C:\\Users\\MAHESAR1\\Desktop\\DEDRR\\P21\\P21 rules\\datasets\\ex.xpt")

lb <- read_xpt("C:\\Users\\MAHESAR1\\Desktop\\DEDRR\\P21\\P21 rules\\datasets\\lb.xpt")

pc <- read_xpt("C:\\Users\\MAHESAR1\\Desktop\\DEDRR\\P21\\P21 rules\\datasets\\pc.xpt")


#################################################################
## Convert datasets into dataframe format to pass the rules
#################################################################

#dm <- as.data.frame(dm)

Rules <- NULL

##########################
## Validation rules
##########################

#rule_1 (SD1001)

SD1001 <- function (data, subject){
  
  total_count <- nrow(data)
  
  x <- unique (subject[duplicated (subject)] )
  
  no_of_subjects_failed <- length (x)
  
   if ( x > 0){
     
     message <-  "Subject's Duplicate Subject ID"
   
     } else {
     
       message <- "No records found"
     }
  
  Rules <- data.frame(Rules = "SD1001", No_of_observations_passed = total_count,  No_of_observations_failed = no_of_subjects_failed,
                    Message = message )
}

Rules <- SD1001 (dm, dm$SUBJID)




#################################################


#rule_2 (SD1334)

SD1334 <- function (data, subject , Informed_consent_date, randomdised_start_date) {
  
  total_count <- nrow(data)
  
  x <- length( unique(subject [ Informed_consent_date  > randomdised_start_date  ]))
  
  
  no_of_subjects_failed <- x
  
  if ( x > 0) {
    
    message <- "Subjects's Informed consent date is after Randomized start date"
    
  } else {
    
    message <- "No records found"
  }
  
  
  Rules <- data.frame(Rules = "SD1334", No_of_observations_passed = total_count,  No_of_observations_failed = no_of_subjects_failed,
                     Message = message )
  
}


Rules <-  rbind (Rules , SD1334 ( dm, dm$SUBJID, dm$RFICDTC , dm$RFSTDTC ) )



################################################

#rule_3 (SD0079)

SD0079 <- function (dm_data, ex_data, dm_arm) {
  
  
  total_count <- length(unique(ex_data$USUBJID))
  
  new <- subset (dm_data, !dm_arm %in% c("SCRNFAIL", "NOTASSGN", "NA"))
  
  a <- unique(new$USUBJID)
  
  b <- unique(ex_data$USUBJID)
  
  x <- length (b[!b %in% a] )
  
  no_of_subjects_failed <- x
  
  if ( x > 0) {
    
    message <- "EX record is present, when subject is not assigned to an arm"
    
  } else {
    
    message <- "No records found"
  }
  
  
  Rules <- data.frame(Rules = "SD0079", No_of_observations_passed = total_count,  No_of_observations_failed = no_of_subjects_failed,
                      Message = message )
  
}


Rules <-  rbind (Rules , SD0079 ( dm, ex, dm$ARMCD ) )


################################################

#rule_4 (SD0040)

SD0040 <- function (lab) {
  
  
  total_count <- length (unique (lab$USUBJID))
  
  lab <- subset (lab, select = c(LBTEST, LBTESTCD))
  
  lab_1 <- unique(lab)
  
  
  lab_1$seq <- 1: nrow(lab_1)
  
  
  lab$seq <- ""
  
  for( i in unique(lab$LBTEST)) {
    
    lab$seq[lab$LBTEST==i] <- lab_1$seq[lab_1$LBTEST==i]
    
  }
  
  
  lab1 <- lab %>%
    select(seq, LBTEST) %>% 
    group_by(seq,LBTEST) %>%
    count()
  
  
  lab2 <- lab %>%
    select(seq, LBTESTCD) %>% 
    group_by(seq,LBTESTCD) %>%
    count()
  
  names(lab2)[3] <- "count"
  
  lab1 <- as.data.frame(lab1)
  
  lab2 <- as.data.frame(lab2)
  
  #lab1$n[1] <- 8880
  
  
  lab3 <- merge(lab1, lab2)
  
  lab3$n <- as.numeric(lab3$n)
  
  lab3$count <- as.numeric(lab3$count)
  
  x <- sum((lab3$n == lab3$count)==F)
  
  no_of_subjects_failed <- x
  
  if ( x > 0) {
    
    message <- "Inconsistent value for LBTEST within LBTESTCD"
    
  } else {
    
    message <- "No records found"
  }
  
  
  Rules <- data.frame(Rules = "SD0040", No_of_observations_passed = total_count,  No_of_observations_failed = no_of_subjects_failed,
                      Message = message )
  
}


Rules <-  rbind (Rules , SD0040 ( lb ) )


###################################################

#rule_5(SD1282)

SD1282 <- function (data){
  
  total_count <- nrow(data)
  
  x <- names(pc)%in% "PCTPTREF" && names(pc) %in% c("PCELTM", "PCTPTNUM", "PCTPT") 
  
  x <- sum(x==F)
  
  no_of_subjects_failed <- x
  
  if ( x > 0){
    
    message <-  "PCTPTREF variable is present when PCELTM, PCTPTNUM, and PCTPT are missing"
    
  } else {
    
    message <- "No records found"
  }
  
  Rules <- data.frame(Rules = "SD1282", No_of_observations_passed = total_count,  No_of_observations_failed = no_of_subjects_failed,
                      Message = message )
}

Rules <- rbind (Rules , SD1282 ( pc ) )


###########################################


########################################################
## Exporting rule_file in a csv
########################################################

write.csv(Rules, "Rule_file.csv", row.names = F)

