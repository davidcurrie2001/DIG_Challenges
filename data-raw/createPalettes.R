# Create our palette csv

df <- data.frame(paletteName = character(),
                 Challenge = character(), 
                 Opportunity = character(), 
                 Issue = character(), 
                 stringsAsFactors=FALSE) 

df <- add_row(df, paletteName = 'RGB', Challenge = '#FF0000', Opportunity = '#008000', Issue ='#0000FF')
df <- add_row(df, paletteName = 'RYB', Challenge = '#FF0000', Opportunity = '#FFFF00', Issue ='#0000FF')
df <- add_row(df, paletteName = 'Grey', Challenge = '#000000', Opportunity = '#A9A9A9', Issue ='#b3b3b3')
df <- add_row(df, paletteName = 'Blue', Challenge = '#90D5FF', Opportunity = '#0000FF', Issue ='#004972')
df <- add_row(df, paletteName = 'Red', Challenge = '#F88379', Opportunity = '#FF0000', Issue ='#A42A04')
df <- add_row(df, paletteName = 'Green', Challenge = '#a7c957', Opportunity = '#6a994e', Issue ='#386641')


write.csv(df, file="data/palettes.csv", row.names = FALSE)

