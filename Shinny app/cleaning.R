df1 <- read.csv("ADHESIVES-JANUARY 2018.csv")
Des <- df1$DESCRIPTION
des_list <- levels(Des)
tmp <- c()
for (var in des_list) {
  strsplit(var,' ')
  tmp<- c(tmp,unlist(strsplit(var,' ')))
}
unique(tmp)
# most frequent words that appear, kinda hard to catagorize products
summary(as.factor(tmp)) 


df1 <- read.csv("JANUARY 2018-des.csv")
unique(df1$title)