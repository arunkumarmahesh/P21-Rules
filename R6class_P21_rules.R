
#############################################################
############## P21_ simple rules
#############################################################

###############################
######## R6 class oop
###############################

##########################
## Installing libraries
##########################

library("haven")

library(R6)

library(XML)

library(methods)

library(plyr)

###########################
## Reaing datasets
###########################

dm <- read_xpt("C:\\Users\\MAHESAR1\\Desktop\\DEDRR\\P21\\P21 rules\\datasets\\dm.xpt")

ex <- read_xpt("C:\\Users\\MAHESAR1\\Desktop\\DEDRR\\P21\\P21 rules\\datasets\\ex.xpt")

lb <- read_xpt("C:\\Users\\MAHESAR1\\Desktop\\DEDRR\\P21\\P21 rules\\datasets\\lb.xpt")

pc <- read_xpt("C:\\Users\\MAHESAR1\\Desktop\\DEDRR\\P21\\P21 rules\\datasets\\pc.xpt")

##########################
## Reading XML file
##########################



##########################
## Validation rules
##########################

#Rule (SD1001)

Rule <- R6Class("Rule", 
                public = list (
                  SUBJID = NULL,
                  
                  initialize = function (SUBJID = unique(SUBJID)){
                    #stopifnot(is.numeric (SUBJID))
                    
                    self$SUBJID <- SUBJID
                  },
                  
                  print = function (...){
                    cat("SD1001: The value of Subject Identifier for the Study (SUBJID) variable must be unique for each subject within the study.\n")
                    cat("SUBJID: ", paste(self$SUBJID[duplicated (self$SUBJID)], sep =","), "\n", sep ="")
                    cat("Message: Duplicate SUBJID")
                    
                    invisible(self)
                  }
                  
                  
                )) 

SD1001 <- Rule$new(dm$SUBJID)

SD1001$print()



#################################

# Rule_1 (SD1334)

dm1 <- dm[!is.na(dm$RFSTDTC) & !dm$RFSTDTC %in% "", ]

Rule_1 <- R6Class("Rule_1",
                  public = list(
                    RFICDTC = NULL,
                    RFSTDTC = NULL,
                    SUBJID = NULL,
                    
                    initialize = function (subject, randoninform , randonstart){
                      self$SUBJID <- subject
                      self$RFICDTC <- randoninform
                      self$RFSTDTC <- randonstart
                      #stopifnot(is.na(RFICDTC))
                      #stopifnot(is.na(RFSTDTC))
                      
                       self$RFICDTC < self$RFSTDTC
                      
                    },
                    
                    print = function (...){
                      cat("SD1334: Date/Time of Informed Consent (RFICDTC) should not be after Subject Reference Start Date/Time (RFSTDTC).\n")
                      cat("SUBJID: ", paste(self$SUBJID[ self$RFICDTC > self$RFSTDTC], collapse =","), "\n", sep ="")
                      cat("Message: RFICDTC (informed consent date) is after RFSTDTC (subject reference start date")
                      
                      invisible(self)
                    }
                  ))


SD1334 <- Rule_1$new (dm1$SUBJID, dm1$RFICDTC, dm1$RFSTDTC)

SD1334$print()


