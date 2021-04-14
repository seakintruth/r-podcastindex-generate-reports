# r-podcastindex-generate-reports
R language script to ingest the podcast index 2.0 database and generate reports.

## What this R script does
Downloads the full podcast database as a sqlite3 file over IPFS from the link available at https://podcastindex.org/

Then generates descriptive analysis reports about the feeds in the database. On a 4 core 8 GB RAM circa 2012 machine this process takes roughly 4-6 minutes to generate the histogram, and boxplot reports.

# Setup
1. Install R (from https://cran.r-project.org/)
1. Install dependencies 
  - On Linux use your distro's equivilent of this ubuntu 20.04 example
  ```bash
  sudo apt install libssl-dev openssl curl libcurl4-openssl-dev libxml2-dev
  ```
1. Download the [generate-podcast-index-reports-from-sqlite3.R](./generate-podcast-index-reports-from-sqlite3.R) script from here
1. Set the executable flag
```
sudo chmod +x ./generate-podcast-index-reports-from-sqlite3.R 
```
# How to run this R script.
1. From the command line (terminal) run
```bash
./generate-podcast-index-reports-from-sqlite3.R 
```
