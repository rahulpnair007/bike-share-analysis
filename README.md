🚲 **Bike Share Analysis Project**<br /><br />
📋 **Project Overview**
This project analyses 12 months of historical trip data from a bike-share company.
The goal is to uncover usage patterns, user behaviour, and provide insights that can help improve operational decisions and marketing strategies.

📈 **Objective**<br />
Compare ride durations between user types.
Identify peak usage days and times.
Analyse differences between bike types.
Discover seasonal trends in ridership.

🛠️ **Tools & Technologies**<br />
Programming Language: R
Libraries: tidyverse, lubridate, janitor

Data Visualisation: ggplot2

Reporting: R Markdown

Version Control: Git & GitHub

🧩 **Steps Performed**<br />
Data Loading: Combined 12 months of .csv trip data files into one dataset.

Data Cleaning: Standardised column names, removed missing values and incorrect records.

Data Transformation: Created new columns such as ride length and day of the week.

Exploratory Data Analysis (EDA):

Ride duration statistics by user type

Bike preference analysis

Usage trends across weekdays and time of day


📚 **Key Findings**<br />
Casual users prefer weekends, while members mostly ride on weekdays.

Classic bikes are the most popular bike type among members.

Casual riders take longer rides on average than members.

Peak ride hours are between 4 PM and 6 PM on weekdays.

🚀 **How to Reproduce**<br />
Clone this repository:

git clone https://github.com/your-username/bike-share-analysis.git
Open the RStudio project.
Run the bike_share_analysis.Rmd file.
Knit to HTML to view the full report.

🤝 **Acknowledgements**
Special thanks to Divvy Bikes for providing the open dataset!
