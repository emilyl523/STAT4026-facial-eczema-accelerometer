```md
# Raw data placement

Place the supplied client data in this folder before running the pipeline.

Expected structure:

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

The raw data are not committed to GitHub because they may contain client-owned study data.
