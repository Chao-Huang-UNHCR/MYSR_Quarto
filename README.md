# MYSR_Quarto

Using R and Quarto to produce online presentations for UNHCR official statistics at regional level

## Structure

- data-raw folder is for data downloaded externally, it includes curated dataset from UNHCR popdata saved by the scripts, world population prospects data.

- data-master folder is for data ready to be called by other scripts, it includes reference data with standard country/region names and codes, a stand alone file with non-displaced stateless data for Asia Pacific (which cannot be downloaded from external sources), GIS maps in png format, and Plotdata folder, which includes all the datasets for the QMD file.

- funtions folder includes the key functions predefined to automate the data processing.

- Data Processing.R to download population data and preprocess the data, it also includes the scripts for TOP COA and COO ranks.

- Plot_prep.R to reshape the POC data into the clean dataset for plotting, all the outputs are saved into Plotdata folder under data-master.

- AP_MYSR_2023.qmd to generate UNHCR branded presentation.

## How to use

- open data-processing.R file, reset the two parameters: MyRegion & MyYear. Currently they are set as 'Asia' and '2023' as default. You can reset to "Europe", "Americas", "EastAfrica", "MENA", "SouthAfrica" or "WestAfrica" based on your need. You will also required to run pd_login() with UNHCT credentials to use the popdata package. Select and RUN the scripts. 

- open Plot_prep.R file, reset the parameters MyRegion, MyYear and plot_title_region.The region and year follow the same in data-process.R, and the plot_title_region can be any characters how you want to call your region. Select and RUN the scripts.

- open AP_MYSR_2023.qmd, reset the parameters MyRegion, MyYear and plot_title_region. Select and RUN the scripts.

- Note the GIS maps, some narratives and operational situations are not applicable for other regions, you will need to modify it manually.

