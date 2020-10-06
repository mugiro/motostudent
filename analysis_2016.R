#
# Load the data of 2016 race

rm(list = ls()) # clean variables
graphics.off() # close graphics windows

require(stringr)
require(readr)
require(dplyr)
require(tibble)
require(purrr)
require(mosaic)

Sys.setenv("DISPLAY" = ":0") # to make View works on vscode

work_dir_gen <- "/Users/andres/Google Drive/14_USAL/2018.19. Academic Info/03.TFG/Sergio Gonzalez Moreno/Informacion MotoStudent UR - Falces"

# Settings
# pattern <- "electric" # Electric bike in 2016
# pattern <- "petrol" # Combustion bike in 2016
pattern <- "log_motostudent_2018_mod" # Electric bike in 2018


if (pattern == "electric") {
    dir_data <- paste(work_dir_gen, "Logs_2016", sep = "/")
    dir_results <- "/results/2016/"
    num_cols <- 7
} else if (pattern == "petrol") {
    dir_data <- paste(work_dir_gen, "Logs_2016", sep = "/")
    dir_results <- "/results/2016/"
    num_cols <- 9
}else if (pattern == "log_motostudent_2018_mod") {
    dir_data <- paste(work_dir_gen, "Logs_2018", sep = "/")
    dir_results <- "/results/2018/"
    num_cols <- 7
}
archivos  <- dir(dir_data)

data_lines  <- read_lines(paste0(dir_data, "/",
                                archivos[which(str_detect(archivos, pattern))])) #, n_max = 2000)
data <- matrix(nrow = length(data_lines), ncol = num_cols)
print(length(data_lines))

for (i in seq_len(length(data_lines))) {
    data[i, ] <- str_split_fixed(data_lines[i], ",", n = num_cols) %>%
                 na_if("") %>%
                 as.numeric()
}

data <- as.data.frame(data)
colnames(data) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
# data <- as_tibble(data) 
print(dim(data))

write_csv(data, paste0(work_dir_gen,
                        dir_results,
                        "To_csv_",
                        archivos[which(str_detect(archivos, pattern))]
                        ))
