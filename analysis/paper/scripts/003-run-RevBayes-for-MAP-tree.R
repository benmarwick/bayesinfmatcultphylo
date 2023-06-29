# run RevBayes file to set up and run model, output is exported as files
# to data directory. I'm using RevBayes v1.2.1, the binary executable is located
# on my computer at /Applications/revbayes-v1.2.1/bin/rb
# The working directory for terminal commands is this RStudio Project

# Start a command with results displayed in a terminal buffer
# do tree inference, make a file path to the script to execute
termId1 <- rstudioapi::terminalExecute('/Applications/revbayes-v1.2.1/bin/rb "analysis/paper/scripts/004-01-RevBayes-for-MAP.Rev"')
termId2 <- rstudioapi::terminalExecute('/Applications/revbayes-v1.2.1/bin/rb "analysis/paper/scripts/004-02-RevBayes-for-MAP.Rev"')
termId3 <- rstudioapi::terminalExecute('/Applications/revbayes-v1.2.1/bin/rb "analysis/paper/scripts/004-03-RevBayes-for-MAP.Rev"')
termId4 <- rstudioapi::terminalExecute('/Applications/revbayes-v1.2.1/bin/rb "analysis/paper/scripts/004-04-RevBayes-for-MAP.Rev"')

# quit the terminal
rstudioapi::terminalKill(termId1)
rstudioapi::terminalKill(termId2)
rstudioapi::terminalKill(termId3)
rstudioapi::terminalKill(termId4)

