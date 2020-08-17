


con <- file("SICB_resubmit.log")
sink(con, append=F)
sink(con, append=F, type="message")

# This will echo all input and not truncate 150+ character lines...
source("C:/Users/Denise Laroze/Documents/GitHub/The-Impact-of-Group-Identity-on-Coalition-Formation/Data analysis/Data_Analysis_GRI_23_jun_2020.R", echo=TRUE, max.deparse.length=10000)

# Restore output to console
sink() 
sink(type="message")

# And look at the log...
cat(readLines("SICB_resubmit.log"), sep="\n")
