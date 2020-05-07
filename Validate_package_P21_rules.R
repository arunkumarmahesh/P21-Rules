
#############################################################
############## P21_ simple rules using validate package
#############################################################

##########################
## Installing libraries
##########################

library("haven")

install.packages ("validate")

library("validate")

install.packages ("dplyr")

library(dplyr)

install.packages("profvis")

library(profvis)

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

dm <- as.data.frame(dm)

##########################
## Validation rules
##########################

#rule_1 (SD1001)

#profvis({

SD1001 <- check_that(dm, is_unique(SUBJID))

new <- summary (SD1001)


new$rule <- "SD1001"

new$message <- "Duplicate SUBJID"

new <- new[, c(9, 1:8, 10)]

barplot (SD1001)

#################################################


#rule_2 (SD1334)


SD1334 <- check_that(dm, RFICDTC < RFSTDTC)

new1 <- summary (SD1334)

new1$rule <- "SD1334"

new1$message <- "RFICDTC is after RFSTDTC"

new1 <- new1[, c(9, 1:8, 10)]


barplot (SD1334)


################################################

#rule_3 (SD0079)

fail <- subset (dm,  ARMCD %in% c("SCRNFAIL", "NOTASSGN", "NA"))

ex1 <- unique(as.numeric(ex$USUBJID))

f1 <- unique(as.numeric(fail$USUBJID))

SD0079 <- check_that(fail, !f1 %in% ex1)

new2 <- summary(SD0079)

new2$rule <- "SD0079"

new2$message <- "EX record is present, when subject is not assigned to an arm"

new2 <- new2[, c(9, 1:8, 10)]


################################################

#rule_4 (SD0040)

lab <- subset (lb, select = c( LBTEST, LBTESTCD))

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

lab1$n[1] <- 8880


lab3 <- merge(lab1, lab2)

lab3$n <- as.numeric(lab3$n)

lab3$count <- as.numeric(lab3$count)

SD0040 <- check_that(lab3, n == count)

new3 <- summary(SD0040)

new3$rule <- "SD0040"

new3$message <- "Inconsistent value for LBTEST within LBTESTCD"

new3 <- new3[, c(9, 1:8, 10)]

###################################################

#rule_5(SD1282)

pc1 <- pc

pc <- as.data.frame(pc)

pc1 <- as.data.frame(pc1)

rules <- validator(names(pc)%in% "PCTPTREF" && names(pc) %in% c("PCELTM", "PCTPTNUM", "PCTPT"))

#names <- c("PCTPTREF", "PCELTM", "PCTPTNUM", "PCTPT")

SD1282 <- confront(pc, rules)

summary(SD1282)

new4 <- summary(SD1282)

new4$rule <- "SD1282"

new4$message <- "PCTPTREF variable is present when PCELTM, PCTPTNUM, and PCTPT are missing"

new4 <- new4[, c(9, 1:8, 10)]

#})


########################################################
## Bind all the rules in a sheet
########################################################

Rule_file <- rbind( new, new1, new2, new3, new4)


write.csv(Rule_file, "Rule_file.csv", row.names = F)
















#########################################################
######## Complex checks
#########################################################

