# https://cran.r-project.org/web/packages/qicharts/vignettes/controlcharts.html

# Load the qicharts package

#install.packages("qicharts")
library(qicharts)

# Lock random number generator to reproduce the charts from this vignette
set.seed(7)

# Create vector of random values to plot
y <- rnorm(24)

# Plot I chart
qic(y, chart = 'i')

# Introduce an outlier at data point number 18
y[18] <- 5

# Plot I chart
qic(y, chart = 'i')

# Setup parameters
m.beds       <- 300
m.stay       <- 4
m.days       <- m.beds * 7
m.discharges <- m.days / m.stay
p.pu         <- 0.08

# Simulate data
discharges  <- rpois(24, lambda = m.discharges)
patientdays <- round(rnorm(24, mean = m.days, sd = 100))
n.pu        <- rpois(24, lambda = m.discharges * p.pu * 1.5)
n.pat.pu    <- rbinom(24, size = discharges, prob = p.pu)
week        <- seq(as.Date('2014-1-1'),
                   length.out = 24, 
                   by         = 'week') 

# Combine data into a data frame
d <- data.frame(week, discharges, patientdays,n.pu, n.pat.pu)
d


qic(n.pu,
    x     = week,
    data  = d,
    chart = 'c',
    main  = 'Hospital acquired pressure ulcers (C chart)',
    ylab  = 'Count',
    xlab  = 'Week')


qic(n.pu, 
    n        = patientdays,
    x        = week,
    data     = d,
    chart    = 'u',
    multiply = 1000,
    main     = 'Hospital acquired pressure ulcers (U chart)',
    ylab     = 'Count per 1000 patient days',
    xlab     = 'Week')


qic(n.pat.pu,
    n        = discharges,
    x        = week,
    data     = d,
    chart    = 'p',
    multiply = 100,
    main     = 'Hospital acquired pressure ulcers (P chart)',
    ylab     = 'Percent patients',
    xlab     = 'Week')


# Create vector of random values from a geometric distribution
d <- c(NA, rgeom(23, 0.08))
d


# Plot G chart
qic(d,
    chart = 'g',
    main  = 'Patients between pressure ulcers (G chart)',
    ylab  = 'Count',
    xlab  = 'Discharge no.')


# Vector of birth weights from 24 babies
y <- round(rnorm(24, mean = 3400, sd = 400))
y

# Plot I chart of individual birth weights
qic(y,
    chart = 'i',
    main  = 'Birth weight (I chart)',
    ylab  = 'Grams',
    xlab  = 'Baby no.')


# Plot moving ranges
qic(y,
    chart = 'mr',
    main  = 'Pairwise differences in birth weights (MR chart)',
    ylab  = 'Grams',
    xlab  = 'Baby no.')


# Vector of 24 subgroup sizes (average = 12)
sizes <- rpois(24, 12)

# Vector of dates identifying subgroups
date <- seq(as.Date('2015-1-1'), length.out = 24, by = 'day')
date <- rep(date, sizes)

# Vector of birth weights
y <- round(rnorm(sum(sizes), 3400, 400))

# Data frame of birth weights and dates
d <- data.frame(y, date)
head(d, 24)

# Plot Xbar chart of average birth weights by date of birth
qic(y, 
    x     = date, 
    data  = d,
    chart = 'xbar',
    main  = 'Average birth weight (Xbar chart)',
    ylab  = 'Grams',
    xlab  = 'Date')


# Plot S chart of within subgroup standard deviation
qic(y, 
    x = date, 
    data = d,
    chart = 's',
    main = 'Standard deviation of birth weight (S chart)',
    ylab = 'Grams',
    xlab = 'Date')


# Pick 24 random dates and sort them
dates  <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), by = 'day')
events <- sort(sample(dates, 24))
events


# Vector of time (days) between events
d <- c(NA, diff(events))
d

# Plot T chart of days between events
qic(d,
    chart = 't',
    main  = 'Days between pressure ulcers (T chart)',
    ylab  = 'Days',
    xlab  = 'Pressure ulcer no.')


# Rebuild data frame from figure 5
d <- data.frame(n.pat.pu, discharges, week)

# Plot standardised P chart
qic(n.pat.pu, 
    n            = discharges,
    x            = week, 
    data         = d,
    chart        = 'p',
    standardised = TRUE,
    main         = 'Patients with hospital acquired pressure ulcers (Standardised P chart)',
    ylab         = 'Standard deviations',
    xlab         = 'Week')
