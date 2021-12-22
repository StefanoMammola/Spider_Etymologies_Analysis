## ------------------------------------------------------------------------
## 'Script to randomly sample species for the cross-validation
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.0.3) and R studio (v. 1.4.1103)

# Loading R packages ------------------------------------------------------

library("xlsx")

# Loading the database ----------------------------------------------------

db <- read.csv(file = "Data/db_initial.csv", header = TRUE, sep = "\t", as.is = FALSE)

# Random sampling ---------------------------------------------------------

total <- 8*50 #amount to sample

names <- c("Stefano",
           "Diego",
           "Nathanel",
           "Atysha",
           "Dylan",
           "Julien",
           "Christophe",
           "Stephen")

db_inferred <- db[db$Source == "Inferred",] ; droplevels(db_inferred)
index <- sample(db_inferred$code) [1 : total]

db_sampled   <- db_inferred[db_inferred$code %in% index,]
db_sampled <- data.frame(Assignment = sort(rep(names, 50)), db_sampled)

# Save
xlsx::write.xlsx(db_sampled,"sampled_cross_validation.xlsx")