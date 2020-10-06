#
# Load the data of 2016 race

rm(list = ls()) # clean variables
graphics.off() # close graphics windows

require(stringr)
require(readr)
require(dplyr)
require(tibble)
require(purrr)
require(ggplot2)
require(forcats)
require(gridExtra)
# require(mosaic)
# require(ggformula)

Sys.setenv("DISPLAY" = ":0") # to make View works on vscode

work_dir_gen <- "/Users/andres/Google Drive/14_USAL/2018.19. Academic Info/03.TFG/Sergio Gonzalez Moreno/Informacion MotoStudent UR - Falces"

# Settings
pattern <- "electric" # Electric bike in 2016
# pattern <- "petrol" # Combustion bike in 2016
# pattern <- "log_motostudent_2018_mod" # Electric bike in 2018


if (pattern == "electric") {
    dir_data <- paste(work_dir_gen, "data/2016", sep = "/")
    dir_results <- "/results/2016/"
    num_cols <- 7
} else if (pattern == "petrol") {
    dir_data <- paste(work_dir_gen, "data/2016", sep = "/")
    dir_results <- "/results/2016/"
    num_cols <- 9
}else if (pattern == "log_motostudent_2018_mod") {
    dir_data <- paste(work_dir_gen, "data/2018", sep = "/")
    dir_results <- "/results/2018/"
    num_cols <- 7
}
archivos  <- dir(dir_data)

data_raw  <- read_csv(paste0(dir_data, "/",
                                archivos[which(str_detect(archivos, pattern))]),
                      col_types = cols(
                        x1 = col_double(),
                        x2 = col_double(),
                        x3 = col_double(),
                        x4 = col_double(),
                        x5 = col_double(),
                        x6 = col_double(),
                        x7 = col_double()))

# summary(data_raw)
if (pattern == "electric") {
    # Remove x7 because it is empty
    data_raw  <- data_raw %>% select(-x7)
}
# Add index to work with the data
data_raw <- data_raw %>% mutate(index = seq_len(n()))

# vars <- paste0("x", "1")
vars <- paste0("x", c(1:6))

colores <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02")

n_i <- 487092
n_f <- 1528776
# n_i <- 1528776 # line of x1_max == 7714737
# n_f <- nrow(data_raw) # 2,274,705

# data <- data_raw %>% slice(., n_i : n_f)
data <- data_raw %>% slice(., (n_i + 1) : n_f)
# data <- data_raw %>% slice(., n_i + 1 : n_f)


for (i in seq_len(length(vars))) {
    data_aux <- data %>% select(index, vars[i]) %>% rename(value = vars[i])

    p <- ggplot(data = data_aux, aes(x = index)) +
                geom_point(aes(y = value), color = colores[i]) +
                # geom_point(aes(y = x2), color = "blue") +
                # scale_y_continuous(expand = c(0, 0), 
                #                   limits = c(0, 2000), 
                #                   breaks = seq(0, 2000, 100)) + 
                theme_bw() +
                theme(strip.placement = "outside",
                        plot.title = element_text(hjust = 0.5),
                        axis.text.x = element_text(angle = 45, hjust = 1),
                        strip.background = element_rect(fill = "#EEEEEE", 
                                                        color = "#FFFFFF"),
                        panel.grid = element_line(color = "#FFFFFF"))
                # xlab(label = "Conditions") +
                # ylab(label = "Primers") +
                # ggtitle(label = label_title)

    ggsave(p, device = "png",
        filename = paste0(work_dir_gen, "/results/2016/", pattern, "_plot_",
                            vars[i], "_", n_i, "-", n_f,
                            ".png"),
        height = 15, width = 120, units = "cm")
}

i <- 1# Value x1 is a counter 
data_aux <- data %>% select(x1) %>% mutate(x1 = as.factor(x1)) %>% mutate(x1 = fct_infreq(x1))

fct_count(data_aux$x1) %>% filter(n < 10)

# ggplot(data = data) +
#             geom_bar(aes(x1)) +
#             # scale_y_continuous(expand = c(0, 0),
#             #            limits = c(0, 400000),
#             #            breaks = seq(0, 400000, 50000)) +
#             coord_flip() +
#             theme_bw()

# There is a potential series of data between 487092 and 1528776



i <- 2 # Value x2
# It looks like there are only few values
# 1] "0"    "128"  "129"  "546"  "547"  "548"  "549"  "643"  "1409" "1537" "1793"
data <- data_raw %>% select(x2) %>% mutate(x2 = as.factor(x2)) %>% mutate(x2 = fct_infreq(x2))

p1 <- ggplot(data = data) +
            geom_bar(aes(x2), color = colores[i]) +
            scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 400000),
                       breaks = seq(0, 400000, 50000)) +
            coord_flip() +
            theme_bw()

# remove the most frequent factors and show the less frequent ones
x2_low <- fct_count(data$x2)  %>% filter(n < 1000) %>% select(f) %>% as_vector() %>% as.character()
# [1] "1409" "1537" "129"  "0"    NA 
data <- data %>% filter(., x2 %in% x2_low)

p2 <- ggplot(data = data) +
            geom_bar(aes(x2), color = colores[i]) +
            scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 500),
                       breaks = seq(0, 500, 50)) +
            coord_flip() +
            theme_bw()

p <- grid.arrange(p1, p2, nrow = 2)

ggsave(p, device = "png",
        filename = paste0(work_dir_gen, "/results/2016/", pattern, "_frequency_",
                            vars[i], "_", n_i, "-", n_f,
                            ".png"),
        height = 25, width = 15, units = "cm")


options(tibble.print_max = Inf) 

data_raw %>% filter(x1 == 105663)
data_raw %>% filter(x1 == 91916)
data_raw %>% filter(x1 == 19524)




data_aux %>% select(value) %>% max()
data_aux %>% filter(value == 158743)

data_raw %>% select(x2) %>% max()

 data %>% 
    group_by(a) %>% 
    summarise(c = names(table(b))[which.max(table(b))])

data_raw %>% select(x2) %>% as.array() %>% 

str(data)
is(data$x2)