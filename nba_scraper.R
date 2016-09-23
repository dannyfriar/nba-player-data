#---- NBA Player Stats Scraper ----#
library(rvest)
library(data.table)

# Blank data.table to hold everything
finalTable <- data.table(team = character(0), name = character(0), points = character(0),
                         rebounds = character(0), assists = character(0), pie = character(0))

# Scrape NBA team list
allTeams <- html('http://www.nba.com/teams/') %>% 
  html_nodes('.nbateamname a') %>%
  html_text()
allTeams <- substr(allTeams, regexpr(' ', allTeams) + 1, nchar(allTeams))
allTeams <- substr(allTeams, regexpr(' ', allTeams) + 1, nchar(allTeams))
allTeams <- tolower(allTeams)
allTeams <- allTeams[!(allTeams == 'mavericks')] # different format - scrape separately
allTeams[allTeams == '76ers'] <- 'sixers'

# Loop through each team
#i <- 1
for(i in 1:length(allTeams)){
  
  print(''); print(paste('Team Index:', i, sep = ' '))
  teamName <- allTeams[i]
  print(teamName)

  # Scrape names of team's players
  teamURL <- html(paste('http://www.nba.com/', teamName, '/roster', sep = ''))
  names <- teamURL %>% html_nodes('.roster__player__header__heading') %>% html_text()
  names <- tolower(names)
  names <- gsub(' ', '_', names)
  names <- gsub('\\.', '', names)
  names <- gsub('\'', '', names)
  names <- names[1:15] # only get main squad
  
  # Create temp table
  tTeam <- rep('', 15); tName <- rep('', 15); tPTS <- rep('', 15); 
  tREB <- rep('', 15); tAST <- rep('', 15); tPIE <- rep('', 15)
  tempTable <- data.table(tTeam, tName, tPTS, tREB, tAST, tPIE)
  tempTable$tTeam <- teamName
  
  # Loop through and get this season's stats for each player
  #idx <- 1
  for(idx in 1:length(names)){
    print(idx)
    
    # URL
    name <- names[idx]
    # remove NAs and deleted player pages
    if(name %in% c('', 'NA', 'marcus_thornton', 'ryan_anderson', 'manu_ginobili', 'tristan_thompson',
                   'glenn_robinson', 'chris_johnson', 'tim_hardaway_jr', 'larry_nance_jr')
       | is.na(name))
    {
      break 
    }
    else if (substr(name, nchar(name), nchar(name)) == '_'){
      name <- substr(name, 1, nchar(name) - 1)
    }
    print(name)
    URL <- html(paste('http://www.nba.com/playerfile/', name, '/', sep = ''))
    tempTable[idx]$tName <- name
    
    # Add to temp table
    pts <- URL %>% html_node('.PTS') %>% html_text
    reb <- URL %>% html_node('.REB') %>% html_text
    ast <- URL %>% html_node('.AST') %>% html_text
    pie <- URL %>% html_node('.PIE') %>% html_text
    tempTable[idx]$tPTS <- gsub(' ', '', pts)
    tempTable[idx]$tREB <- gsub(' ', '', reb)
    tempTable[idx]$tAST <- gsub(' ', '', ast)
    tempTable[idx]$tPIE <- gsub(' ', '', pie)
  }
  
  # Append temp table to final table
  setnames(tempTable, c('tTeam', 'tName', 'tPTS', 'tREB', 'tAST', 'tPIE'),
           c('team', 'name', 'points', 'rebounds', 'assists', 'pie'))
  finalTable <- rbind(finalTable, tempTable)
}


# Clean up data and save as CSV
nbaStats <- finalTable[!(pie == 'N/A')]
nbaStats$pie <- gsub('%', '', nbaStats$pie)
nbaStats <- nbaStats[, lapply(.SD, as.numeric), by = .(team, name)]
nbaStats$pie <- scales::percent(nbaStats$pie/100)
nbaStats <- nbaStats[nchar(name) > 0]
write.csv(nbaStats, 'nbaPlayerStats.csv')
