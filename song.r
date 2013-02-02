library("devtools")
install_github("soundsystem", "abelsonlive")
library("soundsystem")
library("zoo")
notes <- read.csv("/Users/brian/Dropbox/code/soundsystem/data/notes.csv", stringsAsFactors=F)
attach(notes)

# read in data
d <- read.csv("/Users/brian/Dropbox/code/fms-treasury-statements-analysis/fms.day.csv", stringsAsFactors=F)

# rolling z score
z <- function(x) {
    z <- (x-mean(x))/sd(x)
    return(z[length(z)])
}
z_change <- rollapply(d$change, 40, z, align="right")
d <- d[-c(1:39), ]
d$z_change <- z_change
d[is.na(d)] <- 0
summary(d$z_change)
# read in kick and snare:

/Users/brian/Dropbox/code/soundsystem/drums/RolandTR707/
bpm <- 100
k <- readWave("/Users/brian/Dropbox/code/soundsystem/drums/RolandTR707/707_BD0.WAV.wav")
k <- chop(k, bpm, from=0, count="four_")
sn <- readWave("/Users/brian/Dropbox/code/soundsystem/drums/RolandTR707/707_HCP.WAV")
sn <- chop(sn, bpm, count="four_")

# start
song <- prepComb(silence(duration=bpmTime(bpm, "one"), xunit="time"))
for (i in 1400:1765) {
    if(d$change[i]>0) {
        if(d$z_change[i]<(-.6)){
            chord <- min(A3, bpm, "eight_")
        }
        if(d$z_change[i]<(-.37)){
            chord <- min(A4, bpm, "eight_")
        }
        else {
            chord <- min(A2, bpm, "eight_")
        }
    } else {
        if(d$z_change[i] > .27){
            chord <- maj(C3, bpm, "eight_")
        }
        if(d$z_change[i]> .5) {
            chord <- maj(C4, bpm, "eight_")
         }
         else {
            chord <- maj(C2, bpm, "eight_")
         }
    }
    chord <- chop(chord, bpm, count="four_")
    test <- i%%2==0
    if(test){
        sound <- chord + sn
        sound <- prepComb(normalize(sound, unit="16"))
    } else {
        sound <- chord + sn
        sound <- prepComb(normalize(sound, unit="16"))
    }
    song <- bind(song, chord)
    cat(i, "\n")
}
play(song)