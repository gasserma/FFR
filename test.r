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