# R_Project_Marta

# I would like to try to answer to the research question : Does Internet Access in Zambia have an effect on electoral outcomes? 
# Specifically I would be interested to understand if it affects either or both the amount of voters and the direction of the voting outcome, the internet penetration stepped from 3.5% in 2011 to over 11% in 2016 and I have data on elections in 2011 and 2016. 

# To do this I Plan to use three datsets : 

# The first one is an extract from https://electiondataarchive.org/data-and-documentation/clea-lower-chamber-elections-archive/ and is reporting data on lower chamber elections
keeping only election with Zambia as a country and 2011 and 2016 as years. 

# The second dataset is coming from https://electiondataarchive.org/data-and-documentation/georeferenced-electoral-districts-datasets/ and it reports the shape files of the elctoral districts (this data are from 2006 but from a first online research the shape should not have changed too much significantly in the sample period)

#The third dataset is coming from https://opencellid.org/downloads.php and is reporting data on mobile access in different years that I will filter using a timestamp

# The plan for the analysis is to match the electoral districts shapes with the mobile access data and look at the potential correlation of the internet access with electoral outcomes. 

# The plan is to use tidyverse to do the data cleaning and exploration for the first dataset and try to use sr or RGIS to work with the shape files. 


