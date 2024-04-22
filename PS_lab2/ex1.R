vector =  read.csv("life_expect.csv", header=T, sep=',')

female=vector[['female']]
min1=min(female)
max1=max(female)
interval1 = seq (74, 89, 2)
hist(female, breaks=interval1, right=T, freq=F)

male=vector[['male']]
min2=min(male)
max2=max(male)
interval2 = seq (60, 89, 4)
hist(male, breaks=interval2, right=T, freq=F)


