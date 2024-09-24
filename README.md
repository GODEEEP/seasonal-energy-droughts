# seasonal-energy-droughts

1. Download the tgw-gen-historical dataset: https://doi.org/10.5281/zenodo.8393319
2. Unpack the data in a directory of your choice and change the path in `1-preprocess-wind-solar.R`
3. Run `1-preprocess-wind-solar.R` to prepare the wind and solar data.
4. Download the hydropower data: https://doi.org/10.5281/zenodo.13776945
5. Unpack the hydropower data and copy the files `godeeep-hydro-historical-monthly.csv` and `godeeep-hydro-plants.csv` into the `data/` directory, the remaining data is not needed. 
6. Run `2-seasonal-energy-droughts.R` to identify droughts and create the drought data files. The threshold for drought selection can be changed at the top of the file. 
7. Run `3-plots-idf.R` to create a figure related to drought duration and frequency for the paper.
8. (optional) run `threshold-selection.R` to explore the choice of threshold.
9. (optional) run `prepare-gridview-inputs.R` to create input files for the GridView model used for the case study in the paper. 