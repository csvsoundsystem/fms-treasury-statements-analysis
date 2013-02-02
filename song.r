library("devtools")
install_github("soundsystem", "abelsonlive")
library("soundsystem")
library("zoo")
notes = read.csv("/Users/brian/Dropbox/code/soundsystem/data/notes.csv", stringsAsFactors=F)
attach(notes)

# read in data
d <- read.csv("/Users/brian/Dropbox/code/fms-treasury-statements-analysis/fms.day.csv", stringsAsFactors=F)

# rolling z score
z <- function(x) {
    z <- (x-mean(x))/sd(x)
    return(z[length(z)])
}
z_change <- rollapply(d$withdrawals, 100, z, align="right")
d <- d[-c(1:99), ]
d$z_change <- z_change
summary(d$z_change)

# set bpm
bpm <- 120

# read in kick and snare:
k <- readWave("/Users/brian/Dropbox/code/soundsystem/drums/moogKit/Moog BD5.wav")
k <- chop(k, bpm, from=0, count="four_")
sn <- readWave("/Users/brian/Dropbox/code/soundsystem/drums/moogKit/Moog RM5.wav")
sn <- chop(sn, bpm, count="four_")

# start
song <- prepComb(silence(duration=bpmTime(bpm, "one"), xunit="time"))
for (i in 1000:1100) {
    if(d$change[i]>0) {
        if(d$z_change[i]<(-.75)){
            chord <- min(A3, bpm, "four_")
        }
        if(d$z_change[i]<(-1.25)){
            chord <- min(A4, bpm, "four_")
        }
        else {
            chord <- min(A2, bpm, "four_")
        }
    } else {
        if(d$z_change[i] > .75){
            chord <- maj(C3, bpm, "four_")
        }
        if(d$z_change[i]> 1.25) {
            chord <- maj(C4, bpm, "four_")
         }
         else {
            chord <- maj(C2, bpm, "four_")
         }
    }
    chord <- chop(chord, bpm, count="four_")
    if(i%%2==0){
        sound <- chord + sn
        sound <- prepComb(normalize(sound, unit="16"))
    } else {
        sound <- chord + k
        sound <- prepComb(normalize(sound, unit="16"))
    }
    song <- bind(song, chord)
    cat(i, "of", 100, "\n")
}
play(song)