#' various helper functions for generating chords
#'
#' @param base a base note for the chord
#' @param bpm a desired bpm
#' @param count the count of the chord
#' @param tuning what tuning should the chord use
#' @param unit the bit rate to sample with.
#'
#' @return
#' a chopped wave
#'
#' @export
#'
#' @examples
#' #not run
require("tuneR")
interval <- function(interval = 'octave', tuning = '12tet')
{
  # Pythagorean Tuning
  # A = 440
  # A, E, B, F#, C#, G#, D#, A#, F, C, G, D, A
  # A = 440
  # E = 660
  # B = 990 = 495 (9 / 8)
  # F# = 1485 = 742.5 (27 / 16)
  # C# = 2227.5 = 556.875 (81 / 64)
  # G# = 3341.25 = 835.3125 (243 / 128)
  # D# = 5011.875 = 626.4844 (729 / 512)
  # A# = 7517.812 =
  # F = 11276.72
  # C = 16915.08
  # G = 25372.62
  # D = 38058.93
  # A = 57088.39

  pythagorean <- list('unison' = 1,
                      'minor-second' = 256 / 243,
                      'major-second' = 9 / 8,
                      'minor-third' = 32 / 27,
                      'major-third' = 81 / 64,
                      'perfect-fourth' = 4 / 3,
                      'diminished-fifth' = 729 / 512,
                      'perfect-fifth' = 3 / 2,
                      'minor-sixth' = 128 / 81,
                      'major-sixth' = 27 / 16,
                      'minor-seventh' = 16 / 9,
                      'major-seventh' = 243 / 128,
                      'octave'= 2)

  just <- list('unison' = 1,
               'minor-second' = 16 / 15,
               'major-second' = 9 / 8,
               'minor-third' = 6 / 5,
               'major-third' = 5 / 4,
               'perfect-fourth' = 4 / 3,
               'diminished-fifth' = 64 / 45,
               'perfect-fifth' = 3 / 2,
               'minor-sixth' = 8 / 5,
               'major-sixth' = 5 / 3,
               'minor-seventh' = 16 / 9,
               'major-seventh' = 15 / 8,
               'octave'= 2)

  tet <- list('unison' = 1,
              'minor-second' = 2 ^ (1 / 12),
              'major-second' = 2 ^ (2 / 12),
              'minor-third' = 2 ^ (3 / 12),
              'major-third' = 2 ^ (4 / 12),
              'perfect-fourth' = 2 ^ (5 / 12),
              'diminished-fifth' = 2 ^ (6 / 12),
              'perfect-fifth' = 2 ^ (7 / 12),
              'minor-sixth' = 2 ^ (8 / 12),
              'major-sixth' = 2 ^ (9 / 12),
              'minor-seventh' = 2 ^ (10 / 12),
              'major-seventh' = 2 ^ (11 / 12),
              'octave'= 2 ^ (12 / 12))

  if (tuning == 'pythagorean')
  {
    return(pythagorean[[interval]])
  }
  if (tuning == 'just')
  {
    return(just[[interval]])
  }
  else
  {
    return(tet[[interval]])
  }
}

major_scale <- function(note="C3")
{
  notenames = names(df)
  root = which(notenames==note)
  notes =
  c(
    root,
    root + 2,
    root + 4,
    root + 5,
    root + 7,
    root + 9,
    root + 11,
    root + 12
    )
  scale = df[,notes]
  return(scale)
}

minor_scale <- function(note)
{
  notenames = names(df)
  root = which(notenames==note)
  notes =
  c(
    root,
    root + 2,
    root + 3,
    root + 5,
    root + 7,
    root + 9,
    root + 10,
    root + 12
    )
  scale = df[,notes]
  return(scale)
}

bpmTime <- function(bpm=120, count="all"){
  options(digits=10)
  onebar = (60/bpm)*4
  key  =  list(
          thirtytwo = onebar*32,
          sixteen = onebar*16,
          twelve = onebar*12,
          eight = onebar*8,
          six = onebar*6,
          four = onebar*4,
          three = onebar*3,
          two = onebar*2,
          one = onebar,
          two_ = onebar/2,
          three_ = onebar/3,
          four_ = onebar/4,
          six_ = onebar/6,
          eight_ = onebar/8,
          twelve_ = onebar/12,
          sixteen_ = onebar/16,
          thirtytwo_ = onebar/32
        )
  if(count=="all"){
    return(key)
  } else {
    return(as.numeric(key[count]))
  }
}

Maj <- function(base, bpm=bpm, count="four", tuning = '12tet', unit = '16')
{
  if(missing(bpm)){
    stop("You must set bpm")
  }

  time = bpmTime(bpm,count)
  chord = sine(base, duration=time, xunit="time") +
            sine(interval('major-third',
                           tuning = tuning)* base,
                           duration= time,
                           xunit="time")  +
            sine(interval('perfect-fifth',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time")
  chord <- prepComb(normalize(chord, unit=unit))
  return(chord)
}


Min <- function(base, bpm=bpm, count="four", tuning = '12tet', unit = '16')
{
  if(missing(bpm)){
    stop("please set bpm")
  }
  time = bpmTime(bpm,count)
  chord <-  sine(base, duration= time, xunit="time") +
            sine(interval('minor-third',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time") +
            sine(interval('perfect-fifth',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time")
  chord <- prepComb(normalize(chord, unit=unit))
  return(chord)
}

Dim <- function(base, bpm=bpm, count="four", tuning = '12tet', unit = '16')
{
  if(missing(bpm)){
    stop("please set bpm")
  }
  time = bpmTime(bpm,count)
  chord <-  sine(base, duration= time, xunit="time") +
            sine(interval('major-third',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time") +
            sine(interval('diminished-fifth',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time")
  chord <- prepComb(normalize(chord, unit=unit))
  return(chord)
}

Aug <- function(base, bpm=bpm, count="four", tuning = '12tet', unit = '16')
{
  if(missing(bpm)){
    stop("please set bpm")
  }
  time = bpmTime(bpm,count)
  chord <-  sine(base, duration= time, xunit="time") +
            sine(interval('major-third',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time") +
            sine(interval('minor-sixth',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time")
  chord <- prepComb(normalize(chord, unit=unit))
  return(chord)
}

Susp2 <- function(base, bpm=bpm, count="four", tuning = '12tet', unit = '16')
{
  if(missing(bpm)){
    stop("please set bpm")
  }
  time = bpmTime(bpm,count)
  chord <-  sine(base, duration= time, xunit="time") +
            sine(interval('major-second',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time") +
            sine(interval('perfect-fifth',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time")
  chord <- prepComb(normalize(chord, unit=unit))
  return(chord)
}

Susp4 <- function(base, bpm=bpm, count="four", tuning = '12tet', unit = '16')
{
  if(missing(bpm)){
    stop("please set bpm")
  }
  time = bpmTime(bpm,count)
  chord <-  sine(base, duration= time, xunit="time") +
            sine(interval('perfect-fourth',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time") +
            sine(interval('perfect-fifth',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time")
  chord <- prepComb(normalize(chord, unit=unit))
  return(chord)
}
Maj7 <- function(base, bpm=bpm, count="four", tuning = '12tet', unit = '16')
{
  if(missing(bpm)){
    stop("please set bpm")
  }
  time <- bpmTime(bpm,count)
  chord <-  sine(base, duration= time, xunit="time") +
            sine(interval('major-third',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time") +
            sine(interval('perfect-fifth',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time") +
              sine(interval('major-seventh',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time")
  chord <- prepComb(normalize(chord, unit=unit))
  return(chord)
}
Min7 <- function(base, bpm=bpm, count="four", tuning = '12tet', unit = '16')
{
  if(missing(bpm)){
    stop("please set bpm")
  }
  time = bpmTime(bpm,count)
  chord <-  sine(base, duration= time, xunit="time") +
            sine(interval('minor-third',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time") +
            sine(interval('perfect-fifth',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time") +
              sine(interval('minor-seventh',
                           tuning = tuning) * base,
                           duration= time,
                           xunit="time")
  chord <- prepComb(normalize(chord, unit=unit))
  return(chord)
}

loop <- function(object, times=2)
{
  require("tuneR")

    if (!is(object, "Wave"))
        stop("'object' needs to be of class 'Wave' or 'list'")
    validObject(object)
  sound <- object
  for(i in 1:(times-1)){
  sound <- bind(sound, object)
  }
    return(object)
}
chop <- function (object, bpm=120, from=0, count="four")
{
  require("tuneR")

    if (!is(object, "Wave"))
        stop("'object' needs to be of class 'Wave'")
    validObject(object)

    #trim silence
    object = noSilence(object)

    #calculate note length using bpmTime and sample rates
  sr = object@samp.rate
  to = bpmTime(bpm, count) * sr
    lo = length(object@left)
    if (lo < to) {
        silence = silence(duration = to-lo, xunit ="samples")
        object = bind(object, silence)
        return(object)
    }
    return(object[seq(from, to)])
}
