🚲 **Bike Share Analysis Project**<br /><br />
📋 **Project Overview**
This project analyses 12 months of historical trip data from a bike-share company.
The goal is to uncover usage patterns, user behaviour, and provide insights that can help improve operational decisions and marketing strategies.<br /><br />

📈 **Objective**<br />
Compare ride durations between user types.<br />
Identify peak usage days and times.<br />
Analyse differences between bike types.<br />
Discover seasonal trends in ridership.<br />

🛠️ **Tools & Technologies**<br />
Programming Language: R<br />
Libraries: tidyverse, lubridate, janitor<br />
Data Visualisation: ggplot2<br />
Reporting: R Markdown<br />
Version Control: Git & GitHub<br /><br />

🧩 **Steps Performed**<br />
Data Loading: Combined 12 months of .csv trip data files into one dataset.<br />
Data Cleaning: Standardised column names, removed missing values and incorrect records.<br />
Data Transformation: Created new columns such as ride length and day of the week.<br />
Exploratory Data Analysis (EDA):<br />
  Ride duration statistics by user type<br />
  Bike preference analysis<br />
  Usage trends across weekdays and time of day<br /><br />
  
📚 **Key Findings**<br />
Casual users prefer weekends, while members mostly ride on weekdays.<br />
Classic bikes are the most popular bike type among members.<br />
Casual riders take longer rides on average than members.<br />
Peak ride hours are between 4 PM and 6 PM on weekdays.<br /><br />

🚀 **How to Reproduce**<br />
Clone this repository:<br />
git clone https://github.com/your-username/bike-share-analysis.git<br />
Open the RStudio project.<br />
Run the bike_share_analysis.Rmd file.<br />
Knit to HTML to view the full report.<br /><br />

🤝 **Acknowledgements**<br />
Special thanks to Divvy Bikes for providing the open dataset!
