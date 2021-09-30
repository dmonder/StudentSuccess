# StudentSuccess
Peer dashboard generator for student success indicators. This responsitory contains R code and data from the Department of Education intended to 
allow an institutional researcher to quickly create a group of peer institutions for comparison on a number of student success metrics.

The folder structure is:

/StudentSuccess
      /code
      /data
      /peers
      /images
      
The main folder contains an .Rmd file that generates a MS Word document comprising the report. An optional banner image can grace the top of the 
report. To add this, add banner.jpg to the /images folder. Usually you want this to be wide, to stretch across the page.

# Set up
The report requires R and RStudio to run. Install those appropriate to your platform (free), and then run RStudio and at the command prompt
install two libraries needed with 

> install.packages(c("tidyverse","knitr"))

# Specify a peer group
The /peers folder contains R scripts of a particular format. These will specify the peer group either by the IPEDS identifier UNITID or by means
of various filters. For example, you could filter to four-year schools in the South if you like. 

Once this file is created and named in /peers, it is referenced in Report.Rmd here:

> my_peers <- get_peer_group("peer sets/peer set 1.R") %>% left_join(get_dashboard()) 

Instead of "peer set 1.R" you can use whatever peer specification you like. This way you can easily have multiple specifications and 
run a report on each if you like.




