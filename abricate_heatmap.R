#############################################################
# Script usage
#
# Rscript abricate_heatmap.R [input] [output]
# 
# Input is the path to ABRicate summary results in TSV format
# Output is the path to the heatmap image to write to
#############################################################

# load library
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggpubr))

# parse arguments
args <- commandArgs(trailingOnly = T)
input <- args[1]
output <- args[2]

# validate input path
if (!file.exists(input)) {
  message(paste0(input, " does not exist, verify the input path, exiting"))
  quit()
}
# validate output path
if (!file.exists(dirname(output))) {
  message(paste0("The output directory for ", dirname(output), "does not exist, verify the output path, exiting"))
  quit()
}
# load input file
df <- read.delim(input, sep = "\t", header = T, check.names = F)
# rename 1st column of data frame
colnames(df)[1] <- "FILE"
# get number of unique hits
cols <- ncol(df)
# tidy data
tidy_df <- df %>% 
  select(-NUM_FOUND) %>% 
  mutate(FILE = str_replace(FILE, ".*/", ""),
         FILE = str_replace(FILE, ".tab", "")) %>%
  mutate_at(2:ncol(.), ~ifelse(. != ".", "Present", "Absent")) %>% 
  gather(key = "Gene", value = "value", -FILE)
# plot data
p <- tidy_df %>% 
  ggplot(
    aes(x = Gene, 
        y = FILE)) +
  geom_tile(
    aes(fill = factor(value)),
    color = "black"
  ) +
  labs(x = "",
       y = "",
       fill = "") +
  guides(color = "none") +
  theme_bw(10) +
  rotate_x_text(45)
# scale width by number of hits
width_scale <- 14-81/cols
# write image
ggsave(output, p, width = width_scale, height = 8, device = "png")
message(paste0("Wrote heatmap image to: ", output))
  
