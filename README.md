# Tennessee Housing Trends

## Data Sources

### Zillow Home Value Index (ZHVI)

* **Provider:** Zillow Research
* **Description:** Monthly home value index representing the typical home value for homes within the 33rd–67th percentile price range.
* **Geography:** County-level time series filtered to Tennessee
* **Accessed:** 2026-02-27
* **Source:** https://www.zillow.com/research/data/
* **Notes:** Dollar changes computed between Jan 2024 and Jan 2026 using county-level ZHVI data.

### County Boundaries

* **Provider:** United States Census Bureau
* **Dataset:** TIGER/Line cartographic boundary files
* **Accessed via:** R package `tigris`
* **Source:** https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
