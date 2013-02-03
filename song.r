rm(list=ls())
setwd("/Users/brian/Dropbox/code/fms-treasury-statements-analysis/")
qt <- "/Applications/'QuickTime Player.app'/Contents/MacOS/'QuickTime Player'"
setWavPlayer(qt)
library("devtools")
library("tuneR")
library("lubridate")
library("ggplot2")
library("zoo")

# fed rate data
f <- read.csv("fed_rate.csv", stringsAsFactors=F)
f$date <- mdy(f$date)

# dms data
d <- read.csv("fms.day.csv", stringsAsFactors=F)
d$date <- as.Date(d$date)

# join to fed rate
d <- join(f, d, type="right", by="date")

# rolling z scores
z <- function(x){
    (x-mean(x))/sd(x)
}
roll_z <- function(x){
    scores <- z(x)
    scores[length(x)]
}
lag <- 40

z_change <- rollapply(d$change, lag, roll_z)
d <- d[-c(1:(lag-1)), ]
d$z_change <- z_change
N <- nrow(d)
# note data
notes <- read.csv("/Users/brian/Dropbox/code/soundsystem/data/notes.csv", stringsAsFactors=F)

# create melody
white <- grep("\\.", names(notes))
white <- notes[, -white]

# scale to 1:50
scale_vec <- function(x, high, low){
    MIN <- min(x)
    MAX <- max(x)
    laply(x, function(y){
        floor((y - MIN)*(high-low) / (MAX-MIN))
    })
}
rate_notes <- scale_vec(d$rate+1, 50, 2) + 1

source("soundsystem.R")
bpm <- 280
song <- prepComb(silence(duration=bpmTime(bpm, "one"), xunit="time"))
for(i in 1:N){
    print(i)
    freq <- as.numeric(white[, rate_notes[i]])
    sound <- sine(freq, duration=bpmTime(bpm, "four_"), xunit="time")
    sound <- prepComb(normalize(sound, unit="16"))
    song <- bind(song, sound)
}
writeWave(song, "melody.wav")

# chords
attach(notes)
song <- prepComb(silence(duration=bpmTime(bpm, "one"), xunit="time"))

for (i in 1:N) {
    if(d$change[i]<0) {
        if(d$z_change[i]<(-.4)){
            chord <- Min(A2, bpm, "four_")
        }
        if(d$z_change[i]< 0 & d$z_change[i] > (-.4)){
            chord <- Min(A3 , bpm, "four_")
         }
    } else {
        if(d$z_change[i]> 0.4) {
            chord <- Maj(C4, bpm, "four_")
         }
         if(d$z_change[i]>0 & d$z_change[i] < 0.4) {
            chord <- Maj(C3, bpm, "four_")
         }
    }
    chord <- chop(chord, bpm, count="four_")
    test <- i%%2==0
    if(test){
        sound <- chord
        sound <- prepComb(normalize(sound, unit="16"))
    } else {
        sound <- chord
        sound <- prepComb(normalize(sound, unit="16"))
    }
    song <- bind(song, chord)
    cat(i, "\n")
}
writeWave(song, "chords.wav")
