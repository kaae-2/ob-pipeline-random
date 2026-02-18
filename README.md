# Random Baseline Module

## What this module does

Provides a simple random-label baseline for comparison.

- Script: `ob-pipeline-random.R`
- Local runner: `run_random.sh`
- Output: `random_predicted_labels.tar.gz`

It samples labels from the training label set and predicts per test cell.

## Run locally

```bash
bash models/random/run_random.sh
```

## Run as part of benchmark

Included in `benchmark/Clustering_conda.yml` analysis stage and run via:

```bash
just benchmark
```

## What `run_random.sh` needs

- `Rscript` in `PATH`
- R packages used by `ob-pipeline-random.R` (`argparse`, `glue`, `readr`,
  `dplyr`)
- Preprocessing outputs at `models/random/out/data/data_preprocessing/default`
- Writable output directory `models/random/out/data/analysis/default/random`
