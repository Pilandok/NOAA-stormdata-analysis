download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormdata.bz2")
data <- read.csv("stormdata.bz2")
trans <- read.csv("transform1.csv") #ok, I did this part manually in excell, but it should still be reproducible
library(plyr)
View(data)

d1 <- data.frame(EVTYPE=data$EVTYPE, FATALITIES=data$FATALITIES, INJURIES=data$INJURIES)

View(d)
#remove events that did not lead to fatalities
x <- (d$FATALITIES > 0 | d$INJURIES > 0)
d <- d[x,]
write.csv(levels(levels(d$EVTYPE), "transform.csv", row.names=FALSE))

#transform.csv edited in excel to make transform1.csv

trans <- read.csv("transform1.csv")
trans$new.ev <- as.character(trans$new.ev)
#to remove the leading/trailing space added by excel
trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x)  sub("\\s+$", "", x)

for (i in 1:2){for(j in 1:nrow(trans)){(rep1[j,i] <- trim.leading(trans[j,i]))}}
for (i in 1:2){for(j in 1:nrow(trans)){(rep1[j,i] <- trim.trailing(trans[j,i]))}}
trans$new.ev <- as.factor(trans$new.ev)


total.fatalities <- tapply(d$FATALITIES, d$EVTYPE, sum)
total.injuries <- tapply(d$INJURIES, d$EVTYPE, sum)
#replace the event type factor levels
levels(d$EVTYPE) <- trans$new.ev 



#################################################################################################

trans$old.ev <- as.character(trans$old.ev)
trans$new.ev <- as.character(trans$new.ev)

for(i in 1:nrow(d)){
  for(j in 1:nrow(trans)){
    if(d$EVTYPE[i]==trans$old.ev[j]){d$EVTYPE[i] <- trans$new.ev[j]}
  }
}
d$EVTYPE <- as.factor(d$EVTYPE)
d$EVTYPE <- factor(d$EVTYPE)


str(levels(d$EVTYPE))
d$EVTYPE <- as.factor(d$EVTYPE)
#check rows left
sum(x)
y <- as.factor(d$EVTYPE[x])
y <- factor(y)
#check how many levels left
str(levels(y)) #86 levels left

replace(levels(y), c("THUNDERSTORM WIND (G40)", "THUNDERSTORM WINDS"), "THUNDERSTORM WIND" )

replevel <- function(old.ev, new.ev){
    for (ev in old.ev){

         levels(y)[levels(y)==ev]  <- new.ev
    }
    return (levels(y))
    }
  
for (item in 
     
     "THUNDERSTORM WIND"         
     [47] "THUNDERSTORM WIND (G40)"    "THUNDERSTORM WINDS" 
     levels(y) <- replevel(c("THUNDERSTORM WIND (G40)", "THUNDERSTORM WINDS"), "THUNDERSTORM WIND")
     
     as.data.frame(lapply(d, function(EVTYPE) {
       levels(EVTYPE)[levels(EVTYPE) %in% c("THUNDERSTORM WIND (G40)", "THUNDERSTORM WINDS")] <- "THUNDERSTORM WIND"
       EVTYPE
     }))
     
     gsub("[[:punct:]]", " ", x)
     rep1$ev.old <- gsub("\[", "", rep1$ev.old)
     rep1 <- data.frame( gsub("[[:punct:]]", "", rep1))
      rep4 <- rep
      rep4$old.ev <- as.character(rep4$old.ev)
      rep4$new.ev <- as.character(rep4$new.ev)
       for (i in 1:2){for(j in 1:220){(rep4[j,i] <- gsub("[^/[:^punct:]]", "", rep4[j,i]))}}
     for (i in 1:2){for(j in 1:220){(rep4[j,i] <- gsub("[0-9]+", "", rep4[j,i]))}}
      for (i in 1:2){for(j in 1:220){(rep4[j,i] <- trim.leading(rep4[j,i]))}}
      for (i in 1:2){for(j in 1:220){(rep4[j,i] <- trim.trailing(rep4[j,i]))}}
      
      
     for (i in 1:2){for(j in 1:220){(rep1$ev.old[j,i] <- gsub("\[", "", rep1$ev.old[j,i]))}}
     for (i in 1:2){for(j in 1:220){(rep1$ev.new[j,i] <- gsub("\[", "", rep1$ev.new[j,i]))}}
     for (i in 1:2){for(j in 1:220){(rep1$ev.old[j,i] <- gsub("\]", "", rep1$ev.old[j,i]))}}
     for (i in 1:2){for(j in 1:220){(rep1$ev.new[j,i] <- gsub("\]", "", rep1$ev.new[j,i]))}}
 
     
     for (i in 1:2){for(j in 1:220){(rep1[j,i] <- gsub("\\[", "", rep1[j,i]))}}
     for (i in 1:2){for(j in 1:220){(rep1[j,i] <- gsub("\\]", "", rep1[j,i]))}}
     
     for (i in 1:2){for(j in 1:221){(rep1[j,i] <- trim.leading(rep1[j,i]))}}
     trim.leading <- function (x)  sub("^\\s+", "", x)
     trim.leading <- function (x)  sub("^\\s+", "", x)
     
     rep1$ev.old <- gsub("\[", "", rep1$ev.old)
     rep1$ev.new <- gsub("\[", "", rep1$ev.new)
     rep1$ev.old <- gsub("\]", "", rep1$ev.old)
     rep1$ev.new <- gsub("\]", "", rep1$ev.new)
     
     trim.trailing <- function (x) sub("\\s+$", "", x)