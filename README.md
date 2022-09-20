# Funk_List_Collections_Records
Data and code from the paper: 
## Let the records show: attribution of scientific credit in natural history collections
### Annual Report Semantic Search Streamlit Application available here: https://sidatasciencelab.github.io/Funk_List_Collections_Records/

Annual Reports files, both original JPGs, and unprocessed OCR outputs including txt and PDFs, are available on FigShare: https://doi.org/10.25573/data.c.6205081.v1

**Funk_List_Names_RegEx.csv** contains names of deceased Funk List individuals with regular expressions formulas to generate name variants.

**Funk_List_Names_Unit_Department.csv** contains names of deceased Funk List individuals with information on their birth year, Smithsonian unit and department.

**Attributions_groupedby_country_from_Bionomia** directory contains CSV files with country level collection information for the Funk List individuals whose specimen attributions are plotted in Figures 4 and 5. Note that we have made an effort to check that all countries are valid, but there may be instances where, for example, a place is named a country but is not (e.g. Puerto Rico). This is due to outdated database rules and is accounted for in the R scripts used to plot these data.

**Plotting_Rscripts** directory contains R scripts used to generate Figures 1-5.

**Specimen_attributions_from_Bionomia** directory contains 'specimen' CSVs from Bionomia for 40 members of the Funk List with specimen attributions.

**Mrs_Miss_from_GBIF_records.txt** is a list of all unique Mrs., Ms., and Miss entries found in the NMNH Extant Specimens dataset and the NMNH Paleobiology dataset, either in the recordedBy or identifiedBy fields.

**GBIF_occurrence_query.ipynb** contains the code to search for names with regular expressions formulas in recordedBy and identifiedBy fields in a GBIF occurrence file. The NMNH Extant Specimen dataset and NMNH Paleobiology dataset are too large to put on GitHub but can be downloaded with these DOIs:

GBIF.org (23 August 2022) GBIF Occurrence Download https://doi.org/10.15468/dl.92brp2

GBIF.org (23 August 2022) GBIF Occurrence Download https://doi.org/10.15468/dl.a48h5p

