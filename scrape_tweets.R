#!/usr/bin/env Rscript
library(rtweet)
library(optparse)

option_list <- list(
  make_option(c("-d", "--dir"), 
              type="character",
              default=NULL, 
              help="output directory",
              metavar="character")
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

lewontin_tweets <- search_tweets("lewontin", n=10000)

output_file <- paste0(format(min(lewontin_tweets$created_at), format='%Y%m%d'), 
                "-", 
                format(max(lewontin_tweets$created_at), format='%Y%m%d'),
                "_lewontin_tweets.rds")

saveRDS(lewontin_tweets, file.path(opt$dir, output_file))
