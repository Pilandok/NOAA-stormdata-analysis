#NOAA data preprocessing


#create subset for health impact
d1 <- data.frame(EVTYPE=data$EVTYPE, FATALITIES=data$FATALITIES, INJURIES=data$INJURIES)
#remove events that did not lead to fatalities or injuries
#x <- (dd1$FATALITIES > 0 | d$INJURIES > 0)
d1 <- d1[d1$FATALITIES > 0 | d1$INJURIES > 0,]
d1$EVTYPE <- factor(d1$EVTYPE)

#reclassify the levels
event.levels <- as.character(levels(d1$EVTYPE))
event.levels[grep("astro|count|eros|smoke|urban|ash|oth|\\?|drown", event.levels, ignore.case = TRUE)] <- "OTHERS"
event.levels[grep("avalan", event.levels, ignore.case = TRUE)] <- "AVALANCHE"
event.levels[grep("bliz|snow|wint", event.levels, ignore.case = TRUE)] <- "BLIZZARD"
event.levels[grep("frost|freez|cold|glaz|ice|icy|low", event.levels, ignore.case = TRUE)] <- "COLD/FROST"
event.levels[grep("land|mud|rock", event.levels, ignore.case = TRUE)] <- "DEBRIS FLOW"
event.levels[grep("drought", event.levels, ignore.case = TRUE)] <- "DROUGHT"
event.levels[grep("fire|heat|warm|hyper", event.levels, ignore.case = TRUE)] <- "EXCESSIVE HEAT"
event.levels[grep("rain|wet|precip|show|heavy mix", event.levels, ignore.case = TRUE)] <- "EXCESSIVE/HEAVY RAIN"
event.levels[grep("flood|dam|risi", event.levels, ignore.case = TRUE)] <- "FLOOD"
event.levels[grep("fog", event.levels, ignore.case = TRUE)] <- "HEAVY FOG"
event.levels[grep("hail|sleet", event.levels, ignore.case = TRUE)] <- "HAIL"
event.levels[grep("surf|swell|rip|tide|surge|high water|wave", event.levels, ignore.case = TRUE)] <- "HIGH SURF/SWELL"
event.levels[grep("dust|wind|burst|turb|lignt|gust", event.levels, ignore.case = TRUE)] <- "HIGH WIND"
event.levels[grep("hurr|typh|depr", event.levels, ignore.case = TRUE)] <- "HURRICANE"
event.levels[grep("light", event.levels, ignore.case = TRUE)] <- "LIGHTNING"
event.levels[grep("high seas|rough seas|heavy seas", event.levels, ignore.case = TRUE)] <- "MARINE MISHAP"
event.levels[grep(("thund|tstm|storm"), event.levels, ignore.case = TRUE)] <- "THUNDERSTORM"
event.levels[grep(("torn|spout|funn"), event.levels, ignore.case = TRUE)] <- "TORNADO"

levels(d1$EVTYPE) <- as.factor(event.levels)
d1$EVTYPE <- factor(d1$EVTYPE)


#sum up fatalities and injuries

sum.fatal.inj <- ddply(d1, .(EVTYPE), summarize, FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES))
#total.fatalities <- tapply(d1$FATALITIES, d1$EVTYPE, sum)
#total.injuries <- tapply(d1$INJURIES, d1$EVTYPE, sum)
#total.fatalities <- total.fatalities[order(total.fatalities, decreasing = TRUE)]
#total.injuries <- total.injuries[order(total.injuries, decreasing = TRUE)]
top.ftl <- head(order(sum.fatal.inj$FATALITIES, decreasing=TRUE), 10)
top.inj <- head(order(sum.fatal.inj$INJURIES, decreasing=TRUE), 10)

#fatality.injury.table <- sum.fatalities.injuries[top,]

#xthh <- xtable(head(worstHarm),  caption="Top 5 Deadliest or Most Injurious Weather Events")
table1 <- sum.fatalities.injuries[top.fatality,]
table2 <- sum.fatalities.injuries[top.inj,]

table.sum <- data.frame(Fatal_Events = sum.fatal.inj$EVTYPE [top.ftl], Total_Fatalities = sum.fatal.inj$FATALITIES[top.ftl],
                        Injurious_Events = sum.fatal.inj$EVTYPE[top.inj], Total_Injuries = sum.fatal.inj$INJURIES[top.inj])

#create subset for economic impact
d2 <- data.frame(EVTYPE = data$EVTYPE, PROPDMG = data$PROPDMG, PROPDMGEXP = data$PROPDMGEXP, CROPDMG = data$CROPDMG, CROPDMGEXP = data$PROPDMGEXP)
#y <- (d2$PROPDMG > 0 | d2$CROPDMG > 0)
d2 <- d2[d2$PROPDMG > 0 | d2$CROPDMG > 0,]
d2$EVTYPE <- factor(d2$EVTYPE)

#reclassify the levels
event.levels <- as.character(levels(d2$EVTYPE))
event.levels[grep("astro|count|eros|smoke|urban|ash|oth|\\?|drown", event.levels, ignore.case = TRUE)] <- "NON-EVENT"
event.levels[grep("avalan", event.levels, ignore.case = TRUE)] <- "AVALANCHE"
event.levels[grep("bliz|snow|wint", event.levels, ignore.case = TRUE)] <- "BLIZZARD"
event.levels[grep("frost|freez|cold|glaz|ice|icy|low", event.levels, ignore.case = TRUE)] <- "COLD/FROST"
event.levels[grep("land|mud|rock", event.levels, ignore.case = TRUE)] <- "DEBRIS FLOW"
event.levels[grep("drought", event.levels, ignore.case = TRUE)] <- "DROUGHT"
event.levels[grep("fire|heat|warm|hyper", event.levels, ignore.case = TRUE)] <- "EXCESSIVE HEAT"
event.levels[grep("rain|wet|precip|show|heavy mix", event.levels, ignore.case = TRUE)] <- "EXCESSIVE/HEAVY RAIN"
event.levels[grep("flood|dam|risi", event.levels, ignore.case = TRUE)] <- "FLOOD"
event.levels[grep("fog", event.levels, ignore.case = TRUE)] <- "HEAVY FOG"
event.levels[grep("hail|sleet", event.levels, ignore.case = TRUE)] <- "HAIL"
event.levels[grep("surf|swell|rip|tide|surge|high water|wave", event.levels, ignore.case = TRUE)] <- "HIGH SURF/SWELL"
event.levels[grep("dust|wind|burst|turb|lignt|gust", event.levels, ignore.case = TRUE)] <- "HIGH WIND"
event.levels[grep("hurr|typh|depr", event.levels, ignore.case = TRUE)] <- "HURRICANE"
event.levels[grep("light", event.levels, ignore.case = TRUE)] <- "LIGHTNING"
event.levels[grep("high seas|rough seas|heavy seas", event.levels, ignore.case = TRUE)] <- "MARINE MISHAP"
event.levels[grep(("thund|tstm|storm"), event.levels, ignore.case = TRUE)] <- "THUNDERSTORM"
event.levels[grep(("torn|spout|funn"), event.levels, ignore.case = TRUE)] <- "TORNADO"


levels(d2$EVTYPE) <- as.factor(event.levels)
d2$EVTYPE <- factor(d2$EVTYPE)


prp.dmg.lvl <- as.character(levels(d2$PROPDMGEXP))
crp.dmg.lvl <- as.character(levels(d2$CROPDMGEXP))
prp.dmg.lvl[grep(("b"), prp.dmg.lvl, ignore.case = TRUE)] <- "9"
prp.dmg.lvl[grep(("m"), prp.dmg.lvl, ignore.case = TRUE)] <- "6"
prp.dmg.lvl[grep(("k|h"), prp.dmg.lvl, ignore.case = TRUE)] <- "3"
crp.dmg.lvl[grep(("b"), crp.dmg.lvl, ignore.case = TRUE)] <- "9"
crp.dmg.lvl[grep(("m"), crp.dmg.lvl, ignore.case = TRUE)] <- "6"
crp.dmg.lvl[grep(("k|h"), crp.dmg.lvl, ignore.case = TRUE)] <- "3"

levels(d2$PROPDMGEXP) <- as.factor(crp.dmg.lvl)
d2$PROPDMGEXP <- factor(d2$PROPDMGEXP)
levels(d2$CROPDMGEXP) <- as.factor(crp.dmg.lvl)
d2$CROPDMGEXP <- factor(d2$CROPDMGEXP)


#sum up the damage
d2$TOTALDMG <- d2$PROPDMG*10^as.numeric(as.character(d2$PROPDMGEXP)) + d2$CROPDMG*10^as.numeric(as.character(d2$CROPDMGEXP))
d2 <- d2[!is.na(d2$TOTALDMG),]

total.damage <- tapply(d2$TOTALDMG, d2$EVTYPE, sum)
total.damage <- total.damage[order(total.damage, decreasing = TRUE)]

#d2$econ.cost <- (d2$PROPDMG)

#levels(d2$EVTYPE)
#barplot(total.damage)
#str(levels(d2$EVTYPE))
