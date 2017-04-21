#NOAA data preprocessing

d2 <- data.frame(EVTYPE = data$EVTYPE, PROPDMG = data$PROPDMG, PROPDMGEXP = data$PROPDMGEXP, CROPDMG = data$CROPDMG, CROPDMGEXP = data$PROPDMGEXP)
y <- (d2$PROPDMG > 0 | d2$CROPDMG > 0)
d2 <- d2[y,]

#non-event
event.levels <- as.character(levels(d2$EVTYPE))
event.levels[grep("astro|count|eros|smoke|urban|ash|oth|\\?", event.levels, ignore.case = TRUE)] <- "NON-EVENT"
event.levels[grep("avalan", event.levels, ignore.case = TRUE)] <- "AVALANCHE"
event.levels[grep("bliz|snow|wint", event.levels, ignore.case = TRUE)] <- "BLIZZARD"
event.levels[grep("frost|freez|cold|glaz|ice|icy", event.levels, ignore.case = TRUE)] <- "COLD/FROST"
event.levels[grep("land|mud", event.levels, ignore.case = TRUE)] <- "DEBRIS FLOW"
event.levels[grep("drought", event.levels, ignore.case = TRUE)] <- "DROUGHT"
event.levels[grep("fire|heat|warm", event.levels, ignore.case = TRUE)] <- "EXCESSIVE HEAT"
event.levels[grep("rain|wet|precip|show", event.levels, ignore.case = TRUE)] <- "EXCESSIVE/HEAVY RAIN"
event.levels[grep("flood|dam", event.levels, ignore.case = TRUE)] <- "FLOOD"
event.levels[grep("hail", event.levels, ignore.case = TRUE)] <- "HAIL"
event.levels[grep("surf", event.levels, ignore.case = TRUE)] <- "HIGH SURF"
event.levels[grep("dust|wind|burst", event.levels, ignore.case = TRUE)] <- "HIGH WIND"
event.levels[grep("light", event.levels, ignore.case = TRUE)] <- "LIGHTNING"
event.levels[grep("surf", event.levels, ignore.case = TRUE)] <- "MARINE HIGH WIND"
event.levels[grep(("thund|tstm|storm"), event.levels, ignore.case = TRUE)] <- "THUNDERSTORM"
event.levels[grep(("thund|tstm|storm"), event.levels, ignore.case = TRUE)] <- "THUNDERSTORM"
event.levels[grep(("torn|spout|hurri"), event.levels, ignore.case = TRUE)] <- "TORNADO"

levels(d2$EVTYPE) <- as.factor(event.levels)

d2$EVTYPE <- factor(d2$EVTYPE)
levels(d2$EVTYPE)
str(levels(d2$EVTYPE))
