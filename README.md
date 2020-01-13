# ReproducibleResearchPA2
Private repo for the Coursera Reproducible Research Project Assignment #2


## Description of the different scripts\\files

- loadingdata.R load and pre process the raw data
- harmful.R 
    - extract information from pre processed raw data about Fatalities and Injuries grouped by Event Type
    - sort the top 10 most harmfule Event Type 
    - plot Fatalities and Injuries
- costdamage.R 
    - extract information from pre processed raw data about economical damages to Properties and Crop grouped by Event Type
    - sort the top 10 most costly Event Type 
    - plot Prop and Crop costs
- DataProcessing.Rmd notes on how the raw data have been loaded and processed

## Description of overall pipeline

- Loading the data
    - Loading the first rows of the .csv file to consider which variables to load
    - Loading all raws of variables useful to address the questions
- Pre processing data
    - dropping rows unnecessary for the data analysis
    - computing all damage costs using the factor columns
    - filtering and reducing event type to have a better analysis
- Processing data
    - computing sums for costs and casualities and grouping for Event Type
- Showing Results
    - plotting
