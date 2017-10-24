# Analysis of the OPEN-SOURCE database of US clinical trials
## https://clinicaltrials.gov/

Clinical trials are aimed at testing the efficiency of a drug to cure a particular disease on a specific human sample. The design of a clinical trial is a tedious process which implies defining specific research parameters, such as the enrollment number or the eligibility criteria of its sample. To do so, a product owner would need to go manually through the design of previous clinical trials to learn what were the best parameters to bring success to a trial. 
In order to facilitate the work of these product owners, Stéphane MALKA and I decided to build an user-friendly interface which would enable any person to navigate through the 250 000 clinical trials in an efficient way. Stéphane was in charge of designing the interface and I was in charge of the back-end.
This repository shares with you the main bricks of the back-end I built for the application.

## The main bricks of the back-end
### 1. Setting up the database
I set up the database in R (although Python might have been quicker). I extracted all the XML files from the open-source database and aggregated them, making sure that no information was lost. 
Warning: In R this process can be very long especially if you do not have good RAM. 

### 2. Making an exploratory analysis of the database
Then, I analysed the database in a Jupyter Notebook to reshape key variables and create new indicators. 
I needed to work with the library NLTK to create indicators on text variables using NATURAL LANGUAGE PROCESSING. 
I also started some visualisation of the data using the library BOKEH.

### 3. Making a R Shiny application to visualise the database
I created a quick R Shiny app which was meant to be a first POC to be shown to potential prospects. 
Unfortunately Stéphane decided to stop the project at that point, that is why the app is not finished.

## IMPORTANT: the database of clinical trials is not in the repository, if you need it, email me at francois.fitzpatrick@gmail.com ;)
