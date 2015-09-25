# Learning R...

# Create a vector with a function called "c"
x <- c(1,2,3.14,4e2,5/3)

# is that 2e2 for real?
# and, oh God, watch out for the 1 based indices
stopifnot(x[4]==400, "4x10^2 does not equal 400 during the Obama presidency.")

# Everything is an object, but you don't get .Notation
# In fact, according to http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
# you should use '.' in variable name.
a.much.better.variable.name <- x

# Lets test out notion of byval and byref
a.much.better.variable.name[1] <- 2

# Interesting, this fails
stopifnot(x[1]==2)

# More interesting. This fails because the values of all the
# elements are not equal. == doesn't mean what you think it does
stopifnot(x==a.much.better.variable.name)

# Lesson, there don't seem to be any byref/pointer semantics in R

# Memory is managed by a garbage collector that runs automatically
# and keeps track of references, so I think if I want to be nice
# in a large script I might do something like...
a.much.better.variable.name <- NULL

# You get object attributes by remembering a function name :)
length(x)
mode(x)
class(x) # numeric
typeof(x) # double

# It seems like each object can just have data shoved onto it.
# I don't know why...
attr(x, "gasser") <- "the gasser attribute"
attr(x, "gasser")

# I think this is the right way to build a hash table...
y <- vector(mode = "list", length = 5)
names(y) <- c("1", "2", "3", "4", "5")
y["1"] <- 1.1

# This all just adds elements on to the end...
# So I wouln't count on this having O(1) lookup even if it feels like a hash table
y["6"] <- 6.6
y["3.3"] <- 3.33

# This doesn't seem to "print" like I would expect it to. I don't
# think you are supposed to use loops in R...
for (i in length(x)) {
  typeof(x[i])
}

max <- function(a, b) {
  if (a >= b) {
    return(a)
  } else {
    return(b)
  }
}

max(3, 4)
max(5, max(6, 7))

# Lets try recurscion! Although, because there are no
# byref semantics, I can't imagine this being a good way
# to do anything...
fib <- function(n) {
  if (n == 1) {
    return(1)
  } else if (n == 0) {
    return(0)
  } else {
    return(fib(n -1) + fib(n-2))
  }
}

fib(1)
fib(2)
fib(3)
fib(4)
fib(5)



# Stolen from http://fantasyfootballanalytics.net/2014/06/scraping-fantasy-football-projections.html
#Load libraries
library("XML")

#Download fantasy football projections from FantasyPros.com
qb.fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/qb.php", stringsAsFactors = FALSE)$data
rb.fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/rb.php", stringsAsFactors = FALSE)$data
wr.fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/wr.php", stringsAsFactors = FALSE)$data
te.fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/te.php", stringsAsFactors = FALSE)$data

names(qb.fp)

my.qb.fp <- list(qb.fp[["Player"]], qb.fp[["FPTS"]])

names(my.qb.fp)

# lets truncate, because I really don't care about BJ Daniels projection...
# lets take everyone with more than 5 points projected
my.qb.fp[[2]][my.qb.fp[[2]] <= 5]

# whoops, nothing is working because these are characters...
typeof(my.qb.fp[[2]][1])
my.qb.fp[[2]] <- as.numeric(my.qb.fp[[2]])
typeof(my.qb.fp[[2]][1])


my.qb.fp[[2]] <- my.qb.fp[[2]][my.qb.fp[[2]] > 5]

# This feels so wrong... I'm assigning to the result of a function call...? but it seems to work
# This only makes sense to me if you think of a lot of these function calls as goofy ass ways to write getters/setters
length(my.qb.fp[[1]]) <- length(my.qb.fp[[2]])
names(my.qb.fp) <- c("Player", "FPTS")

# Lets try to find Nick Foles projection
my.qb.fp$FPTS[grep(my.qb.fp$Player, pattern = "Foles")]


# lets try some optimization?!
# todo, I really want to locally save the projections so they don't change
# just for sanity with all this testing.
library("Rglpk")

# reading here: https://www.math.washington.edu/~burke/crs/407/notes/section1.pdf

# for this practice optimization, we are going to try to find the optimal one QB team
# in a world where all the quarterbacks cost the same

# the objective function, these are coefficients in the form:
# c1x1 + c2x2 ...
# so for us, these are the quarterback projections
obj <- c(my.qb.fp$FPTS)

# need to model the contraints, basically say that we only get to have 1 QB
mat <- matrix(rep(1, 32), nrow = 1)

dir <- c("<=")
rhs <- c(1)
max <- TRUE
single.qb.team <- Rglpk::Rglpk_solve_LP(obj, mat, dir, rhs, max = max)

# in this blindingly simple problem we should never not choose the first option.
stopifnot(single.qb.team$solution[1]==1)

# and just because its Dangeruss right now, lets print out the name
my.qb.fp$Player[single.qb.team$solution == 1]

# some of this seems wrong. I think the optimization function smells wrong, who knows.

# lets create a simple scenario, 1 qb, 2 rb, 3 wr team, with fake projections, and prices, and a price limit
qb.proj <- c(20,15,18)
qb.price <- c(3,1,2)
rb.proj <- c(10,12,14,9,1,8,14,11,25)
rb.price <- c(6,10,5,9,9,9,9,9,75)
wr.proj <- c(10,9,8,10,9,8,10,9,8,12,12,26)
wr.price <- c(5,5,8,8,8,8,8,8,8,8,8,76)

# with price limit 100, I think this looks like qb[1]+rb[3]+rb[1]+wr[12]+wr[1]+wr[2]

# so, we need to optimize for points, and constrain the positions and total price
# max 20qb1+15qb2+18qb3+10rb1+12rb2+14rb3+9rb4+1rb5+8rb6+14rb7+11rb8+25rb9+10wr1+9wr2+8wr3+10wr4+9wr5+8wr6+10wr7+9wr8+8wr9+12wr10+12wr11+26wr12
# with constraints 
#   3qb1+1qb2+2qb3+6rb1+10rb2+5rb3+9rb4+9rb5+9rb6+9rb7+9rb8+75rb9+5wr1+5wr2+8wr3+8wr4+8wr5+8wr6+8wr7+8wr8+8wr9+8wr10+8wr11+76wr12 <= 100
#   1qb1+1qb2+1qb3 <= 1
#   1rb1+1rb2+1rb3+1rb4+1rb5+1rb6+1rb7+1rb8+1rb9 <= 2
#   1wr1+1wr2+1wr3+1wr4+1wr5+1wr6+1wr7+1wr8+1wr9+1wr10+1wr11+1wr12 <= 3
#
# okay, so this is easy right?
obj <- c(qb.proj, rb.proj, wr.proj)

row1 <- c(qb.price, rb.price, wr.price)
row2 <- c(1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
row3 <- c(0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)
row4 <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1)

mat <- matrix(data = c(row1,row2,row3,row4), nrow = 4, ncol = length(row1), byrow = TRUE)
dir <- c("<=","==","==","==")
rhs <- c(100,1,2,3)
max <- TRUE
types <- c(rep("B", length(row1)))
simple.team <- Rglpk::Rglpk_solve_LP(obj, mat, dir, rhs, max = max, types = types)

# super cool, the output here is :
#$optimum
#[1] 89
#
#$solution
#[1] 1 0 0 1 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 1
#
#$status
#[1] 0
# this means that our optimum team is projected for 89 points, and consists of qb[1]+rb[3]+rb[1]+wr[12]+wr[1]+wr[2]
# note that the 0 means it thinks it found the optimal solution. I don't know exactly when it wouldn't...
