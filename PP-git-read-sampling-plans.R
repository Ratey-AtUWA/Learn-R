# run this line first ONCE
source("https://github.com/Ratey-AtUWA/Learn-R/raw/main/PP-initialise-sampling-plans.R")

# Then run the following lines of code together, changing values in `input`
# as required

input <- data.frame(xysp=25,     # grid spacing (m)
                    ifsq=FALSE,  # FALSE for triangular, TRUE for rectangular
                    off0=15,     # grid offset
                    rand0=0)     # randomisation distance
source("https://github.com/Ratey-AtUWA/Learn-R/raw/main/PP-make-sampling-plans.R")
