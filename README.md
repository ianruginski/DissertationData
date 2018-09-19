# DissertationData
Dissertation Data and Data Cleaning Scripts
**README**

The primary materials for this project have been included in a link to a GitHub repository, which contains data, data cleaning scripts (for R), and data analysis scripts (also for R). The files included in the GitHub are described in the following list...

 - **DistanceRaw.csv**: Distance estimation raw data from virtual SILCton, one of the measures used to create a visual learning latent factor.
 - **MobilitySurvey_RScript1.3.R**: File for cleaning mobility and survey data (in file Mobility_Survey_Version_1.3_IanDiss.csv). However what is needed from this file for cleaning has been included in the Ruginski_Dissertation_DataCleaning.csv file also in the repo, this file is just here for working separately if desired. 
 - **Mobility_Survey_Version_1.3_IanDiss.csv**: Mobility and survey raw data from Qualtrics. 
 - **MRTRaw.csv**: Vandenberg Kuse mental rotation raw data from virtual SILCton. Scoring system described in manuscript.
 - **ModelBuildingRaw.csv**: Cognitive mapping raw data from virtual SILCton, one of the measures used to create a visual learning latent factor. Did not actually load with the visual learning latent factor.
 - **OffSitePointingRaw.csv**: An above-ground pointing measure raw data from virtual SILCton. This was not used in any analyses, but might be interesting for future work.
 - **OnSitePointingRaw.csv**: Pointing error raw data from virtual SILCton. Used to create within and between route pointing measures as part of a visual learning latent factor.
 - **PerspectiveTakingDataRaw.csv**: Pointing error raw data from Koheznikov and Hegarty's (2001) pencil and paper perspective taking task. Scored from paper and pencil tests in the lab by myself and research assistants. See manuscript for more on scoring.
 - **Ruginski_Diss_Analysis.R**: Analyses used in paper, with commented code for reproducability of analyses. Also includes cleaning of perspective taking data (PerspectiveTakingDataRaw.csv) at beginning of file.
 - **Ruginski_Dissertation_DataCleaning.R**: Data cleaning script for all files except perspective taking raw data, which is included in Ruginski_Diss_Analysis.R. This should be done prior to running analyses using the script. 
 - **ian_diss_cleaneddatawithpt.csv**: Final cleaned datafile in csv format that can easily be used with most statistical analysis software. 
 
Steps for analysis:

1. Clean data using Ruginski_Dissertation_Datacleaning.R
2. Finish cleaning data using first part of Ruginski_Diss_Analysis.R . Can also do analyses using this file or modify as needed
