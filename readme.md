Given some data created by
[fms-treasury-statements](https://github.com/csvsoundsystem/fms-treasury-statements),
make something cool.

Load data from capitol words

    ./capitolwords-download.sh spending
    ./capitolwords-insert.py spending

## Questions

What spending shoots up in the middle of the month?

Why do we have this bimodal distribution? Why does spending stop at
$50 billion, and what causes it to jump past that?

## For the wiki

We used various R sonification, processing and visualization libraries to produce
our multisensory information interactive. These libraries include

* plyr
* reshape2
* csvsoundsystem
* aplpack

Me sought to produce an interactive that simultaneously displayed highly dimensional
data. We started with a 55-dimensional dataset

* 1 date
* 52 daily line items
* 1 daily interest rate
* 1 debt ceiling

We added a few variables to assist with our analyses.

* Day of week
* Day of month
* Rolling mean
* Rolling z-score
* Daily balance
* Variance of the 52 line items

And then reduced the now-61-dimensional dataset to an interactive.

We used principal component analysis to rotate the 52 line-items, and we plotted
the 15 highest-loaded components as Chernoff faces. We plotted interest rate and
balance, with an error width.

We represented similar data in audio. We selected chords based on the derivative
of cash balance, and we composed a melody based on the interest rate.
