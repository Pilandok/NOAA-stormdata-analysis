#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormdata.bz2")
data <- read.csv("stormdata.bz2")
View(data)
d <- data.frame(EVTYPE=data$EVTYPE, FATALITIES=data$FATALITIES, INJURIES=data$INJURIES)
View(d)
#remove events that did not lead to fatalities
x <- (d$FATALITIES != 0 & d$INJURIES != 0)
#check rows left
sum(x)
y <- as.factor(d$EVTYPE[x])
y <- factor(y)
#check how many levels left
str(levels(y)) #86 levels left



Astronomical Low Tide Z
Avalanche Z
Blizzard Z
Coastal Flood Z
Cold/Wind Chill Z
Debris Flow C
Dense Fog Z
Dense Smoke Z
Drought Z
Dust Devil C
Dust Storm Z
Excessive Heat Z
Extreme Cold/Wind Chill Z
Flash Flood C
Flood C
Frost/Freeze Z
Funnel Cloud C
Freezing Fog Z
Hail C
Heat Z
Heavy Rain C
Heavy Snow Z
High Surf Z
High Wind Z
Hurricane (Typhoon) Z
Ice Storm Z
Lake-Effect Snow Z
Lakeshore Flood Z
Lightning C
Marine Hail M
Marine High Wind M
Marine Strong Wind M
Marine Thunderstorm Wind M
Rip Current Z
Seiche Z
Sleet Z
Storm Surge/Tide Z
Strong Wind Z
Thunderstorm Wind C
Tornado C
Tropical Depression Z
Tropical Storm Z
Tsunami Z
Volcanic Ash Z
Waterspout M
Wildfire Z
Winter Storm Z
Winter Weather Z 