rm(list=ls())
setwd("/Users/brian/Dropbox/code/fms-treasury-statements-analysis/")
library("tuneR")
library("plyr")
library("lubridate")
library("ggplot2")
library("zoo")
library("devtools")
qt <- "/Applications/'QuickTime Player.app'/Contents/MacOS/'QuickTime Player'"
setWavPlayer(qt)

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
bpm <- 280
notes <- read.csv("/Users/brian/Dropbox/code/soundsystem/data/notes.csv", stringsAsFactors=F)

# take only the "White Keys"
white <- grep("\\.", names(notes))
white <- notes[, -white]

# scale rate from 10:50
scale_vec <- function(x, high, low){
    MIN <- min(x)
    MAX <- max(x)
    laply(x, function(y){
        floor((y - MIN)*(high-low) / (MAX-MIN))
    })
}
rate_notes <- scale_vec(d$rate+1, 50, 13) + 12


source("soundsystem.R")
melody <- prepComb(silence(duration=bpmTime(bpm, "one"), xunit="time"))
for(i in 1:length(rate_notes)) {
    freq <- as.numeric(white[,rate_notes[i]])
    sound <- sine(freq, duration=bpmTime(bpm, "four_"), xunit="time")
    sound <- prepComb(normalize(sound, unit="16"))
    sound <- chop(sound, bpm, count="four_")
    melody <- bind(melody, sound)
    print(i)
}
writeWave(melody, "melody_final.wav")



debt_notes <- scale_vec(d$dist_to_debt+1, 50, 13) + 12


source("soundsystem.R")
melody <- prepComb(silence(duration=bpmTime(bpm, "one"), xunit="time"))
for(i in 1:length(rate_notes)) {
    freq <- as.numeric(white[,rate_notes[i]])
    sound <- sine(freq, duration=bpmTime(bpm, "four_"), xunit="time")
    sound <- prepComb(normalize(sound, unit="16"))
    sound <- chop(sound, bpm, count="four_")
    melody <- bind(melody, sound)
    print(i)
}
writeWave(melody, "melody_final.wav")


# chords
d$id <- 1:nrow(d)
attach(notes)
chords <- prepComb(silence(duration=bpmTime(bpm, "one"), xunit="time"))
for(i in 1:nrow(d)){
    if(d$change[i]<0) {
        if(d$z_change[i]<(-.4)){
            chord <- Min(A2, bpm, "four_")
        }
        if(d$z_change[i]< 0 & d$z_change[i] > (-.4)){
            chord <- Min(A3 , bpm, "four_")
         }
    } else {
        if(d$z_change[i] > 0.4) {
            chord <- Maj(C4, bpm, "four_")
         }
         if(d$z_change[i] > 0 & d$z_change[i] < 0.4) {
            chord <- Maj(C3, bpm, "four_")
         }
    }
    chord <- chop(chord, bpm, count="four_")
    chord <- prepComb(normalize(chord, unit="16"))
    chords <- bind(chords, chord)
    print(i)
}
writeWave(chords, "chords_final.wav")
