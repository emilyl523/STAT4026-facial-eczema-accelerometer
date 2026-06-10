```md
# Raw data

Place the supplied raw study data in this folder before running the pipeline.

The raw data are not committed to GitHub because they may contain client-owned study records and large accelerometer files.

## Expected folder structure

The pipeline expects the following structure:

```text
data/raw/
├── Accelerometer data - FG/
├── Accelerometer data - IM/
├── Accelerometer data - OS/
└── Accelerometer data - WL/
    ├── WL results_GGT and GLDH.xlsx
    ├── Set 1/
    │   ├── Set 1 - collar numbers.xlsx
    │   └── ...
    └── Set 2/
        ├── Set 2 - collar numbers.xlsx
        └── ...
```
The WL folder is required for the final analysis. The other programme folders are used for archive screening and file inventory summaries.

## Required WL files

The WL analysis requires:

```{text}
data/raw/Accelerometer data - WL/WL results_GGT and GLDH.xlsx
data/raw/Accelerometer data - WL/Set 1/Set 1 - collar numbers.xlsx
data/raw/Accelerometer data - WL/Set 2/Set 2 - collar numbers.xlsx
```

The accelerometer CSV files should remain inside their original Set 1 and Set 2 subfolders.

## Alternative raw data location

If the raw data are stored outside the GitHub repository, set the environment variable `FE_RAW_DATA_DIR` before running the pipeline.

For example:
```{r}
Sys.setenv(FE_RAW_DATA_DIR = "/path/to/raw/data")
source("R/run_pipeline.R")
```

The directory supplied to `FE_RAW_DATA_DIR` should contain the same programme folders shown above.
