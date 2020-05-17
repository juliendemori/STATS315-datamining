#Reading in the data
california = read.table('California_Data.txt', sep=',')
names(california) <- c('medval', 'medinc', 'houseage', 'averoom', 'avebedroom', 'popul', 'aveoccup', 'latitude', 'longitude')
medValue = california[,1]
x = california[,2:9]
x = as.matrix(x)

#Making all the variables numeric
type = c(1,1,1,1,1,1,1,1,1)

#Running mart
mart(x,medValue,type, niter = 800)
moremart()

progress()

#Seeing significance of variables
dev.new()
par(mar=c(4,6,4,4)+0.1)
varimp()

#Singleplots and pairplots
dev.new()
singleplot(1)
dev.new()
singleplot(6)
dev.new()
pairplot(1,8)



