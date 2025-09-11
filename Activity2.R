#script for fall25 homework 2

#ACTIVITY 2

#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm

heights[1]

#look at R's built-in descriptions of matrix function uses
help(matrix)

#byrow determines the way in which matrix values are arranged
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#look at particular values within the matrix's grid
Mat.bycol[1,2]

#dataframes act similarly to matrices, but allow for colums with different
#forms of media (numbers, characters, etc)
