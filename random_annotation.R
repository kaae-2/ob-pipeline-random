#!/usr/bin/env/R

## Omnibenchmark-izes Marek Gagolewski's https://github.com/gagolews/clustering-results-v1/blob/eae7cc00e1f62f93bd1c3dc2ce112fda61e57b58/.devel/do_benchmark_fcps_aux.R

## Takes the true number of clusters into account and outputs a 2D matrix with as many columns as ks tested,
## being true number of clusters `k` and tested range `k plusminus 2`


library(argparse)
library(FCPS)
## library(R.utils)

parser <- ArgumentParser(description="FCPS caller")

# parser$add_argument('--data.matrix',
#                     type="character",
#                     help='gz-compressed textfile containing the comma-separated data to be clustered.')
# parser$add_argument('--data.true_labels',
#                     type="character",
#                     help='gz-compressed textfile with the true labels.')
parser$add_argument('--train.data.matrix',
                    type="character",
                    help='gz-compressed textfile containing the comma-separated data to be clustered.')
parser$add_argument('--labels_train',
                    type="character",
                    help='gz-compressed textfile with the true labels.')
parser$add_argument('--seed',
                    type="integer",
                    help='Random seed',
                    default = 819797,
                    dest = 'seed')
parser$add_argument("--output_dir", "-o", dest="output_dir", type="character",
                    help="output directory where files will be saved", default=getwd())
parser$add_argument("--name", "-n", dest="name", type="character", help="name of the dataset")
# parser$add_argument("--method", "-m", dest="method", type="character", help="method")

args <- parser$parse_args()

load_labels <- function(data_file) {
  (fd <- read.table(gzfile(data_file), header = FALSE, quote = "\'", na.strings = '""')$V1)
}

load_dataset <- function(data_file) {
  (fd <- read.table(gzfile(data_file), header = TRUE, sep = ","))
}

# 1) Shuffle of true labels
# do_fcps <- function(truth, seed) {
# 
#   # Randomly assign a class to each cell
#   res <- sample(truth) 
#   
#   res_char <- as.character(res)
#   
#   res_char <- paste0(res_char, ".0")
#   
#   res_char[res_char == "NA.0"] <- NA
# 
#   return(res_char)
#   
# }

# 2) Sample from unique true labels
do_fcps <- function(truth, n_cells, seed) {

  # Randomly assign a class to each cell
  res <- unique(truth)

  res_char <- as.character(res)

  res_char <- paste0(res_char, ".0")

  res_char[res_char == "NA.0"] <- NA
  
  res_final <- sample(res_char, n_cells, replace = TRUE)

  return(res_final)

}

# truth <- load_labels(args[['data.true_labels']])
truth <- load_labels(args[['labels_train']])

# only for 2
# data <- load_dataset(args[['data.matrix']]) 
data <- load_dataset(args[['train.data.matrix']]) 
n_cells <- nrow(data)

# res <- do_fcps(truth = truth, seed = args$seed) # 1
res <- do_fcps(truth = truth, n_cells = n_cells, seed = args$seed) # 2

outfile <- file.path(args[['output_dir']], paste0(args[['name']], "_predicted_labels.txt"))
write.table(file = outfile, res, col.names = FALSE, row.names = FALSE, quote = FALSE, na = '""')


# TEST 1)
# truth <- read.table(gzfile("Documents/courses/Benchmarking/data/true_labs.txt.gz"), header = FALSE, quote = "\'", na.strings = '""')$V1
# res <- do_fcps(truth = truth, seed = 66)
# outfile <- file.path("Downloads", "Bla_predicted_labels.txt")
# write.table(file = outfile, res, col.names = FALSE, row.names = FALSE, quote = FALSE, na = '""')


# TEST 2)
# truth <- read.table(gzfile("Documents/courses/Benchmarking/data/true_labs.txt.gz"), header = FALSE, quote = "\'", na.strings = '""')$V1
# data <- read.table(gzfile("Documents/courses/Benchmarking/data/data_matrix.matrix.gz"), header = TRUE, sep = ",")
# n_cells <- nrow(data)
# res <- do_fcps(truth = truth, n_cells = n_cells, seed = 66)
# outfile <- file.path("Downloads", "Bla_predicted_labels.txt")
# write.table(file = outfile, res, col.names = FALSE, row.names = FALSE, quote = FALSE, na = '""')

