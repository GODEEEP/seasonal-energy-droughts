# seasonal-energy-droughts

1. Download the tgw-gen-historical dataset: https://doi.org/10.5281/zenodo.8393319
2. Unpack the data in a directory of your choice and change the path in `1-preprocess-wind-solar.R`
3. Run `1-preprocess-wind-solar.R` to prepare the wind and solar data.
4. Download the hydropower data: https://doi.org/10.5281/zenodo.13776945
5. Unpack the hydropower data and copy the files `godeeep-hydro-historical-monthly.csv` and `godeeep-hydro-plants.csv` into the `data/` directory, the remaining data is not needed. 
6. Run `2-seasonal-energy-droughts.R` to identify droughts and create the drought data files. The threshold for drought selection can be changed at the top of the file. 
7. Run the script/notebooks `3a`, `3b`, `3c`, `3d` to create figures for the paper. 
8. Run the `4a` notebook to conduct the cluster analysis and produce the figure for the paper.
9. Download the [composite meteorology data](https://zenodo.org/records/14270835/files/meteo_composite_data.zip?download=1) 
10. In the `4b` notebook modify the path to the data
11. Run the `4b` notbook to conduct the composite analysis and produce figures for the paper.
12. Run the `4c` notbook to conduct the correlation analysis and produce figures for the paper.
13. Download the [case study data](https://zenodo.org/records/14270835/files/case-study.zip?download=1) 
14. Unpack the data and change the data path in the `5-case-study.py` script.
15. Run the `5-case-study.py` notbook to analyse the case study data and produce figures for the paper.
16. (optional) run `threshold-selection.R` to explore the choice of threshold.
17. (optional) run `prepare-gridview-inputs.R` to create input files for the GridView model used for the case study in the paper. 