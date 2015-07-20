---
title: "Reproducible Research Assignment 2: The Effects of Weather Events on Human Health and Economy"
author: "Cheng-Han Yu"
date: "July 19, 2015"
output: html_document
---

# Synopsis
The basic goal of this project is to explore the U.S. National Oceanic and Atmospheric Administration NOAA Storm Database from 1950 to November 2011 and address the following questions:

* Across the United States, which types of events (as indicated in the `EVTYPE` variable) are most harmful with respect to population health?

* Across the United States, which types of events have the greatest economic consequences?

The data show that insbsolute numner, `TORNADO` is the event that affected human health the most, while `FLOOD` is the event that damages US economy most severely.


# Data Loading and Processing

I first download the data from the course website and save it in my R working directory, and then use `read.csv()` to load the data into R.

```r
storm <- read.csv("repdata-data-StormData.csv")
str(storm)
head(storm)
names(storm)
```
Functions `str()`, `head` and `names()` are handy to see a general picture of the data. There are 902297 observations with 37 variables. To answer our questions in this project, eight variables are kept and stored in a new dataset `st`.

There are many typos in variable `EVTYPE`, for instance, mispelled TORNDAO and many different recorded types actually mean the same weather event, for example, FLASH FLOOD and FLOOD/FLASH are the same thing, TSTM is actually equivalent to THUNDERSTORM. Moreover, some types are written in lowercase, but others in uppercase. The following code does those a little tedius text processing, grouping the same weather events as precisely as possible. It is based on the Storm Data Event Table on page 6 of [NATIONAL WEATHER SERVICE INSTRUCTION](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) provided. After doing so, there are 262 different types left, much less than the original number of types, 985. 


```r
st <- storm[, c("STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", 
                   "PROPDMGEXP","CROPDMG", "CROPDMGEXP")]
st$EVTYPE <- factor(toupper(st$EVTYPE))
# levels(st$EVTYPE)
trim <- function(x) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}
st$EVTYPE <- factor(trim(st$EVTYPE))
# levels(st$EVTYPE)
st[grepl("BLIZZARD", st$EVTYPE), ]$EVTYPE <- "BLIZZARD"
st[grepl("AVALANCE", st$EVTYPE), ]$EVTYPE <- "AVALANCHE"
st[grepl("BRUSH FIRE", st$EVTYPE), ]$EVTYPE <- "BRUSH FIRE"
st[grepl("BLOW-OUT TIDE", st$EVTYPE), ]$EVTYPE <- "BLOW-OUT TIDE"
st[grepl("DRY", st$EVTYPE), ]$EVTYPE <- "DRY"
st[grepl("DROUGHT", st$EVTYPE), ]$EVTYPE <- "DROUGHT"
st[grepl("COASTAL FLOOD", st$EVTYPE), ]$EVTYPE <- "COASTAL FLOOD"
st[(grepl("COLD", st$EVTYPE) & !(grepl("EXTREME COLD", st$EVTYPE))
   & !(grepl("EXTREME/RECORD COLD", st$EVTYPE))), ]$EVTYPE <- "COLD"
st[grepl("EXTREME/RECORD COLD", st$EVTYPE), ]$EVTYPE <- "EXTREME COLD"
st[grepl("DUST STORM", st$EVTYPE), ]$EVTYPE <- "DUSTSTORM"
st[grepl("DUST DEVEL", st$EVTYPE), ]$EVTYPE <- "DUST DEVIL"
st[grepl("FLASH FLOOD", st$EVTYPE), ]$EVTYPE <- "FLASH FLOOD"
st[grepl("FLOOD FLASH", st$EVTYPE), ]$EVTYPE <- "FLASH FLOOD"
st[grepl("FLOOD/FLASH", st$EVTYPE), ]$EVTYPE <- "FLASH FLOOD"
st[(grepl("FLOOD", st$EVTYPE) & !(grepl("FLASH FLOOD", st$EVTYPE)) &
        !(grepl("LAKESHORE FLOOD", st$EVTYPE)) & 
        !(grepl("COASTAL FLOOD", st$EVTYPE))), ]$EVTYPE <- "FLOOD"
st[grepl("FUNNEL", st$EVTYPE), ]$EVTYPE <- "FUNNEL CLOUD"
st[grepl("FROST", st$EVTYPE), ]$EVTYPE <- "FROST/FREEZE"
st[grepl("GLAZE", st$EVTYPE), ]$EVTYPE <- "GLAZE"
st[grepl("HAIL", st$EVTYPE) & !(grepl("MARINE HAIL", st$EVTYPE)), ]$EVTYPE <- "HAIL"
st[grepl("HEAT", st$EVTYPE) & !(grepl("EXCESSIVE HEAT", st$EVTYPE)), ]$EVTYPE <- "HEAT"
st[grepl("HEAVY RAIN", st$EVTYPE), ]$EVTYPE <- "HEAVY RAIN"
st[grepl("HEAVY PRECIPATATION", st$EVTYPE), ]$EVTYPE <- "HEAVY PRECIPITATION"
st[grepl("HIGH  SWELLS", st$EVTYPE), ]$EVTYPE <- "HIGH SWELLS"
st[grepl("HEAVY SHOWERS", st$EVTYPE), ]$EVTYPE <- "HEAVY SHOWER"
st[grepl("HVY RAIN", st$EVTYPE), ]$EVTYPE <- "HEAVY RAIN"
st[grepl("HEAVY SNOW", st$EVTYPE), ]$EVTYPE <- "HEAVY SNOW"
st[grepl("HIGH SURF", st$EVTYPE), ]$EVTYPE <- "HIGH SURF"
st[grepl("HIGH WIND", st$EVTYPE) & !(grepl("MARINE HIGH WIND", st$EVTYPE)), ]$EVTYPE <- "HIGH WIND"
st[grepl("HURRICANE", st$EVTYPE), ]$EVTYPE <- "HURRICANE"
st[grepl("HYPOTHERMIA/EXPOSURE", st$EVTYPE), ]$EVTYPE <- "HYPOTHERMIA"
st[grepl("ICE ON ROAD", st$EVTYPE), ]$EVTYPE <- "ICE ROADS"
st[grepl("ICY ROADS", st$EVTYPE), ]$EVTYPE <- "ICE ROADS"
st[grepl("LANDSLIDES", st$EVTYPE), ]$EVTYPE <- "LANDSLIDE"
st[grepl("LAKE EFFECT SNOW", st$EVTYPE), ]$EVTYPE <- "LAKE-EFFECT SNOW"
st[grepl("LIGHTNING", st$EVTYPE), ]$EVTYPE <- "LIGHTNING"
st[grepl("LIGHTING", st$EVTYPE), ]$EVTYPE <- "LIGHTNING"
st[grepl("LIGNTNING", st$EVTYPE), ]$EVTYPE <- "LIGHTNING"
st[grepl("LOW TEMPERATURE RECORD", st$EVTYPE), ]$EVTYPE <- "LOW TEMPERATURE"
st[grepl("MUD SLIDE", st$EVTYPE), ]$EVTYPE <- "MUDSLIDE"
st[grepl("MUDSLIDE/LANDSLIDE", st$EVTYPE), ]$EVTYPE <- "MUDSLIDE"
st[grepl("MUD/ROCK SLIDE", st$EVTYPE), ]$EVTYPE <- "MUDSLIDE"
# st[grepl("RAIN (HEAVY)", st$EVTYPE), ]$EVTYPE <- "HEAVY RAIN"
st$EVTYPE[289860] <- "HEAVY RAIN"
st[grepl("RIP CURRENT", st$EVTYPE), ]$EVTYPE <- "RIP CURRENT"
st[grepl("RECORD/EXCESSIVE HEAT", st$EVTYPE), ]$EVTYPE <- "EXCESSIVE HEAT"
st[grepl("SMALL STREAM AND", st$EVTYPE), ]$EVTYPE <- "SMALL STREAM"
st[grepl("SML STREAM FLD", st$EVTYPE), ]$EVTYPE <- "SMALL STREAM"
st[grepl("TORRENTIAL RAINFALL", st$EVTYPE), ]$EVTYPE <- "TORRENTIAL RAIN"

st[grepl("THUNDERSTORM WIND", st$EVTYPE) & 
       !(grepl("MARINE THUNDERSTORM WIND", st$EVTYPE)), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("THUDERSTORM WINDS", st$EVTYPE), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("THUNDEERSTORM WINDS", st$EVTYPE), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("THUNDERESTORM WINDS", st$EVTYPE), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("THUNDERSTORM", st$EVTYPE) &
       !(grepl("MARINE THUNDERSTORM WIND", st$EVTYPE)), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("TUNDERSTORM", st$EVTYPE), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("THUNDERSTROM", st$EVTYPE), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("THUNDERTSORM", st$EVTYPE), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("THUNDERTORM", st$EVTYPE), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("THUNDESTORM", st$EVTYPE), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("THUNERSTORM", st$EVTYPE), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("SNOW", st$EVTYPE), ]$EVTYPE <- "SNOW"
st[grepl("SLEET", st$EVTYPE), ]$EVTYPE <- "SLEET"
st[grepl("STORM SURGE", st$EVTYPE), ]$EVTYPE <- "STORM SURGE"
st[grepl("TYPHOON", st$EVTYPE), ]$EVTYPE <- "HURRICANE"
st[grepl("TSTM", st$EVTYPE), ]$EVTYPE <- "THUNDERSTORM WIND"
st[grepl("TORNADO", st$EVTYPE), ]$EVTYPE <- "TORNADO"
st[grepl("TORNDAO", st$EVTYPE), ]$EVTYPE <- "TORNADO"
st[grepl("UNSEASONABLY WARM YEAR", st$EVTYPE), ]$EVTYPE <- "UNSEASONABLY WARM"
st[grepl("UNUSUAL/RECORD WARMTH", st$EVTYPE), ]$EVTYPE <- "UNUSUAL WARMTH"
st[grepl("UNUSUALLY WARM", st$EVTYPE), ]$EVTYPE <- "UNUSUAL WARMTH"
st[grepl("URBAN", st$EVTYPE), ]$EVTYPE <- "URBAN SMALL"
st[grepl("UNSEASONABLY WARM & WET", st$EVTYPE), ]$EVTYPE <- "UNSEASONABLY WARM/WET"
st[grepl("WET MICOBURST", st$EVTYPE), ]$EVTYPE <- "WET MICROBURST"
st[grepl("VOLCANIC ASH", st$EVTYPE), ]$EVTYPE <- "VOLCANIC ASH"
st[grepl("WATERSPOUT", st$EVTYPE), ]$EVTYPE <- "WATERSPOUT"
st[grepl("WATER SPOUT", st$EVTYPE), ]$EVTYPE <- "WATERSPOUT"
st[grepl("WAYTERSPOUT", st$EVTYPE), ]$EVTYPE <- "WATERSPOUT"
st[grepl("TROPICAL STORM", st$EVTYPE), ]$EVTYPE <- "TROPICAL STORM"
st[grepl("WILDFIRE", st$EVTYPE), ]$EVTYPE <- "WILDFIRE"
st[grepl("WILD FIRES", st$EVTYPE), ]$EVTYPE <- "WILDFIRE"
st[grepl("WILD/FOREST FIRE", st$EVTYPE), ]$EVTYPE <- "WILDFIRE"
st[grepl("WINTER STORM", st$EVTYPE), ]$EVTYPE <- "WINTER STORM"
st[grepl("WINTER WEATHER", st$EVTYPE), ]$EVTYPE <- "WINTER WEATHER"
st[(grepl("WIND", st$EVTYPE) &
       !(grepl("MARINE THUNDERSTORM WIND", st$EVTYPE)) &
       !(grepl("THUNDERSTORM WIND", st$EVTYPE))), ]$EVTYPE <- "WIND"
st[grepl("WND", st$EVTYPE), ]$EVTYPE <- "WIND"
st[grepl("WINTERY MIX", st$EVTYPE), ]$EVTYPE <- "WINTER MIX"
st[grepl("WINTRY MIX", st$EVTYPE), ]$EVTYPE <- "WINTER MIX"
st$EVTYPE <- trim(st$EVTYPE)
```

# Results

## Human Health 

### Fatalities
We first examine which weather events caused the most fatalities from 1950 to November 2011. 

```r
fatal <- aggregate(FATALITIES ~ EVTYPE, data = st, sum)
freq <- as.matrix(table(st$EVTYPE))
fatal <- cbind(fatal, freq, rate = fatal$FATALITIES / freq)
fatal_order <- fatal[order(fatal$FATALITIES, decreasing = TRUE), ]
head(fatal_order)
```

```
##                              EVTYPE FATALITIES   freq        rate
## TORNADO                     TORNADO       5636  60699 0.092851612
## EXCESSIVE HEAT       EXCESSIVE HEAT       1920   1681 1.142177275
## HEAT                           HEAT       1212    950 1.275789474
## FLASH FLOOD             FLASH FLOOD       1035  55676 0.018589698
## LIGHTNING                 LIGHTNING        817  15777 0.051784243
## THUNDERSTORM WIND THUNDERSTORM WIND        715 329868 0.002167534
```

```r
fatal_rate_order <- fatal[order(fatal$rate, decreasing = TRUE), ]
head(fatal_rate_order)
```

```
##                      EVTYPE FATALITIES freq     rate
## MARINE MISHAP MARINE MISHAP          7    2 3.500000
## ROUGH SEAS       ROUGH SEAS          8    3 2.666667
## TSUNAMI             TSUNAMI         33   20 1.650000
## HEAVY SEAS       HEAVY SEAS          3    2 1.500000
## HEAT                   HEAT       1212  950 1.275789
## HYPOTHERMIA     HYPOTHERMIA          8    7 1.142857
```

```r
(sum(fatal$FATALITIES >= 1)) # number of types causing deaths
```

```
## [1] 69
```


The figure below shows that `TORNADO`, `(EXCESSIVE) HEAT`, `FLASH FLODD`, `LIGHTNING` and `THUNDERSTORM` caused the most fatalities. But this may be due to high occurrences of those events. If we check the fatality rate, the number of fatalities / the number of event occured, the first three events with high fatality rate are `MARINE MISHAP`, `ROUGH SEAS`, and `TSUNAMI`. These events caused more than one death on average once they happen.

```r
library(ggplot2)
theme <- theme(axis.text.x = element_text(colour = "black", size = 12,
                                     angle = 90), 
          axis.text.y = element_text(colour = "grey20", size = 12),
          axis.title.x = element_text(size = 12, hjust = .5),
          axis.title.y = element_text(size = 12, hjust = .5, vjust = 1),
          title = element_text(size = 14, hjust = .5)) +
    theme(legend.position = "none")
g <- ggplot(fatal_order[1:20, ], aes(reorder(EVTYPE, -FATALITIES), FATALITIES))
g <- g + geom_bar(stat = "identity", aes(fill = FATALITIES)) + 
    xlab("Event Type") + ylab("Number of Fatalities") + 
    ggtitle('Top 20 Fatalities by Event Type') + theme
g_rate <- ggplot(fatal_rate_order[1:20, ], aes(reorder(EVTYPE, -rate), rate))
g_rate <- g_rate + geom_bar(stat = "identity", aes(fill = rate)) + 
    xlab("Event Type") + ylab("Fatalities / Event Occured") + 
    ggtitle('Top 20 Fatality Rate by Event Type') + theme
multiplot(g, g_rate, cols = 2)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

### Injuries
We also check the number of injuries caused by the weather events. Again, both total injuries and injury rates are examined. The top five events that caused the most injuries are `TORNADO`, `THUNDERSTORM`, `FLOOD`, `EXCESSIVE HEAT` and `LIGHTNING`, which is similar to the fatality case. If we turn into injury rate, `TSUNAMI` `EXCESSIVE RAINFALL`, `GLAZE` and `HURRICANE(TYPHOON)` are more likely to cause injuries. 


```r
injury <- aggregate(INJURIES ~ EVTYPE, data = st, sum)
injury_order <- injury[order(injury$INJURIES, decreasing = TRUE),]
g_in <- ggplot(injury_order[1:20, ], aes(reorder(EVTYPE, -INJURIES), INJURIES))
g_in <- g_in + geom_bar(stat = "identity", aes(fill = INJURIES)) + 
    xlab("Event Type") + ylab("Number of Injuries") + 
    ggtitle('Top 20 Injuries by Event Type') + theme
freq <- as.matrix(table(st$EVTYPE))
injury <- cbind(injury, freq, rate = injury$INJURIES / freq)
injury_rate_order <- injury[order(injury$rate, decreasing = TRUE), ]
(sum(injury$INJURIES>=1)) # number of types causing injuries
```

```
## [1] 63
```

```r
g_in_rate <- ggplot(injury_rate_order[1:20, ], aes(reorder(EVTYPE, -rate), rate))
g_in_rate <- g_in_rate + geom_bar(stat = "identity", aes(fill = rate)) + 
    xlab("Event Type") + ylab("Injuries / Event Occured") + 
    ggtitle('Top 20 Injury Rate by Event Type') + theme
multiplot(g_in, g_in_rate, cols = 2)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

## Economic Consequences

### Property and Crop Damages

To calculate the damage expenses correctly, we first convert the recorded damage numbers into expenses in dollar unit, based on page 12 of NATIONAL WEATHER SERVICE INSTRUCTION. The figure below shows that `FLOOD` and `HURRICANE` caused lots of damages on properties, while `DROUGHT` and `FLOOD` resulted in huge damages on crops. If we combine property and crop damages together, we see that `FLOOD` caused the most economic loss.

```r
unique(st$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(st$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```

```r
idx <- c("", "+", "-", "?", 0:9, "h", "H", "k", "K", "m", "M", "b", "B");
digit <- c(rep(0,4), 0:9, 2, 2, 3, 3, 6, 6, 9, 9)
multi <- data.frame(idx, digit)
st$damage.p <- st$PROPDMG * 10 ^ multi[match(st$PROPDMGEXP, multi$idx), 2]
st$damage.c <- st$CROPDMG * 10 ^ multi[match(st$CROPDMGEXP, multi$idx), 2]
st$damage <- st$damage.p + st$damage.c
dmg.p <- aggregate(damage.p ~ EVTYPE, data = st, sum)
dmg.p_order <- dmg.p[order(dmg.p$damage.p, decreasing = TRUE), ]
dmg.c <- aggregate(damage.c ~ EVTYPE, data = st, sum)
dmg.c_order <- dmg.c[order(dmg.c$damage.c, decreasing = TRUE), ]
dmg <- aggregate(damage ~ EVTYPE, data = st, sum)
dmg_order <- dmg[order(dmg$damage, decreasing = TRUE), ]
g.p <- ggplot(dmg.p_order[1:10, ], aes(reorder(EVTYPE, -damage.p), damage.p))
g.p <- g.p + geom_bar(stat = "identity", aes(fill = damage.p)) + 
    xlab("Event Type") + ylab("Property Damage ($)") + 
    ggtitle('Top 10 Property Damage') + theme
g.c <- ggplot(dmg.c_order[1:10, ], aes(reorder(EVTYPE, -damage.c), damage.c))
g.c <- g.c + geom_bar(stat = "identity", aes(fill = damage.c)) + 
    xlab("Event Type") + ylab("Crop Damage ($)") + 
    ggtitle('Top 10 Crop Damage') + theme
g.t <- ggplot(dmg_order[1:10, ], aes(reorder(EVTYPE, -damage), damage))
g.t <- g.t + geom_bar(stat = "identity", aes(fill = damage)) + 
    xlab("Event Type") + ylab("Total Damage ($)") + 
    ggtitle('Top 10 Total Damage') + theme
multiplot(g.p, g.c, g.t, cols = 3)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

# Conclusion
This project finds the most harmful weather events on human health and economic damages. To save human lifes and health, we should be careful when a tornado is coming. If we want to minimize our economic loss, either properties or crops, due to a natural disaster, definitely do something when we hear a flood alarm.   

