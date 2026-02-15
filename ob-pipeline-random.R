#!/usr/bin/env/R

## Omnibenchmark-izes Marek Gagolewski's https://github.com/gagolews/clustering-results-v1/blob/eae7cc00e1f62f93bd1c3dc2ce112fda61e57b58/.devel/do_benchmark_fcps_aux.R

## Takes the true number of clusters into account and outputs a 2D matrix with as many columns as ks tested,
## being true number of clusters `k` and tested range `k plusminus 2`

if (!requireNamespace("argparse", quietly = TRUE)) {
  install.packages("argparse", repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(argparse)
  library(glue)
  library(readr)
  library(dplyr)
  library(utils)
})

message("Random: starting")
# GET ARGUMENTS 
parser <- ArgumentParser(description="FCPS caller")

parser$add_argument('--data.train_matrix',
                    type="character",
                    help='gz-compressed textfile containing the comma-separated data to be clustered.')
parser$add_argument('--data.train_labels',
                    type="character",
                    help='gz-compressed textfile with the true labels.')
parser$add_argument('--data.test_matrix',
                    type="character",
                    help='gz-compressed textfile containing the comma-separated data to be clustered.')
parser$add_argument('--data.label_key',
                    type="character",
                    help='label key metadata path (accepted but unused).')
# parser$add_argument('--data.test_labels',
#                     type="character",
#                     help='gz-compressed textfile with the true labels.')
parser$add_argument("--output_dir", "-o", dest="output_dir", type="character",
                    help="output directory where files will be saved", default=getwd())
parser$add_argument("--name", "-n", dest="name", type="character", help="name of the dataset")
# parser$add_argument("--method", "-m", dest="method", type="character", help="method")

args <- parser$parse_args()

train_x_path <- args[['data.train_matrix']]
train_y_path <- args[['data.train_labels']]
test_x_path <- args[['data.test_matrix']]
name <- args[['name']]
output_dir <- args[['output_dir']]
output_dir <- normalizePath(output_dir, mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# FOR TESTING
# Path to zipped data
# dataset_path <- "/Users/srz223/Documents/courses/Benchmarking/repos/ob-pipeline-cytof/out/data/data_import/dataset_name-FR-FCM-Z2KP_virus_final_seed-42/preprocessing/data_preprocessing/num-1_test-sample-limit-5"
# train_x_path <- glue("{dataset_path}/data_import.train.matrix.tar.gz")
# train_y_path <- glue("{dataset_path}/data_import.train.labels.tar.gz")
# # test_y_path <- glue("{dataset_path}/data_import.test.labels.tar.gz")
# test_x_path <- glue("{dataset_path}/data_import.test.matrices.tar.gz")

# LOAD TRAIN Y
train_y_files <- utils::untar(train_y_path, list = TRUE)

# extract to a temp dir
tmp <- tempdir()
utils::untar(train_y_path, exdir = tmp)

truth <- character()
for (file in train_y_files) {
  df <- read_csv(file.path(tmp, file), col_names = FALSE, show_col_types = FALSE)
  truth <- c(truth, as.character(df[[1]]))
  rm(df)
}

# LOAD X TEST 
test_x_files <- utils::untar(test_x_path, list = TRUE)

# extract to a temp dir
tmp <- tempdir()
utils::untar(test_x_path, exdir = tmp)

# Sample from unique true labels
do_random <- function(label_pool, n_cells) {
  sample(label_pool, n_cells, replace = TRUE)
}

count_rows <- function(path) {
  wc <- tryCatch(
    suppressWarnings(system2("wc", c("-l", path), stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  if (length(wc) > 0L) {
    parsed <- suppressWarnings(as.integer(strsplit(trimws(wc[1]), "\\s+")[[1]][1]))
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  # Fallback for environments where wc is unavailable.
  con <- file(path, open = "r")
  on.exit(close(con), add = TRUE)
  n <- 0L
  repeat {
    lines <- readLines(con, n = 100000L, warn = FALSE)
    if (length(lines) == 0L) {
      break
    }
    n <- n + length(lines)
  }
  n
}

get_sample_number <- function(file_name, fallback) {
  base <- basename(file_name)
  base <- gsub("\\.csv(\\.gz)?$", "", base)
  m <- regexpr("[0-9]+(?!.*[0-9])", base, perl = TRUE)
  if (m[1] == -1) {
    return(as.character(fallback))
  }
  substr(base, m[1], m[1] + attr(m, "match.length") - 1)
}

tmp_dir <- file.path(tempdir(), sprintf("random-%d", Sys.getpid()))
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
# tmp_dir <- "~/Documents/courses/Benchmarking/repos/ob-pipeline-random/tmp_dir"
csv_files <- character(length(test_x_files))
names(csv_files) <- test_x_files
label_pool <- unique(truth)
set.seed(101)
  
# Run random classification on each test sample.
for (i in seq_along(test_x_files)) {
  test_x_name <- test_x_files[i]
  
  # test_x_name <- "data_import-data-14.csv"
  n_cells <- count_rows(file.path(tmp, test_x_name))
  pred_y <- do_random(label_pool = label_pool, n_cells = n_cells)
  
  if (length(pred_y) != n_cells){
    message("Random: mismatch between predictions and test rows")
  }
  
  sample_number <- get_sample_number(test_x_name, i)
  csv_file <- file.path(tmp_dir, sprintf("%s-prediction-%s.csv", name, sample_number))
  
  # If the element is a data.frame or list, coerce to data.frame
  df <- as.data.frame(pred_y)
  
  write_delim(df, file = csv_file, col_names = FALSE, quote = "none", delim = ",")
  csv_files[test_x_name] <- csv_file
  rm(pred_y, df)
  
}

# Create tar.gz archive of all CSVs
# name <- "random"
# output_dir <- "~/Documents/courses/Benchmarking/repos/ob-pipeline-random/tmp_out"
old_wd <- getwd()
setwd(tmp_dir)
tar(
  tarfile = {
    tar_path <- file.path(output_dir, sprintf("%s_predicted_labels.tar.gz", name))
    dir.create(dirname(tar_path), recursive = TRUE, showWarnings = FALSE)
    tar_path
  },
  files = basename(csv_files),
  compression = "gzip",
  tar = "internal"
)
setwd(old_wd)

message("Random: done")


# TEST 1)
# truth <- read.table(gzfile("Documents/courses/Benchmarking/data/true_labs.txt.gz"), header = FALSE, quote = "\'", na.strings = '""')$V1
# res <- do_fcps(truth = truth, seed = 66)
# outfile <- file.path("Downloads", "Bla_predicted_labels.txt")
# write.table(file = outfile, res, col.names = FALSE, row.names = FALSE, quote = FALSE, na = '99')


# TEST 2)
# truth <- read.table(gzfile("Documents/courses/Benchmarking/data/true_labs.txt.gz"), header = FALSE, quote = "\'", na.strings = '""')$V1
# data <- read.table(gzfile("Documents/courses/Benchmarking/data/data_matrix.matrix.gz"), header = TRUE, sep = ",")
# n_cells <- length(truth)
# res <- do_fcps(truth = truth, n_cells = n_cells, seed = 66)
# length(res)
# na_label <- res %>% as.integer() %>% na.exclude() %>% max() + 1
# outfile <- file.path("Downloads", "NEW_predicted_labels.txt")
# write.table(file = outfile, res, col.names = FALSE, row.names = FALSE, quote = FALSE, na = as.character(na_label))

# data <- read.table(outfile, header = FALSE)
# dim(data)
