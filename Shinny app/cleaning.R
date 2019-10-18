df1 <- read.csv("Collection_data.csv")
title <- df1$title
Product <- c('ADHESIVE',
             'ENZYMES',
             'EPOXY',
             'GELATIN',
             'GLUE',
             'MODIFIED STARCH',
             'PROTEIN',
             'SEAL',
             'SEALANT',
             'STARCH',
             'TAPE',
             'OTHERS')
for (var in 1:length(title)) {
  if(title[var] %in% Product){
  }
  else{
    title[var] = 'OTHERS'
  }
}
df1$title = title
# most frequent words that appear, kinda hard to catagorize products
summary(as.factor(title)) 
df1 = droplevels.data.frame(df1)

write.csv(df1,"Collection_data.csv")