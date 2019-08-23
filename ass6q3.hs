import Data.List 

one:: [[Integer]]
one = group [2*i+1|i<-[1..5]]

two:: [(Integer,Bool)]
two = [(5*j,j==4)|j<-[1..5],j==1||j>3]

four::[(Integer,Integer)]
four= [(i,j)|i<-[1..5],j<-[5,4..1],j>i]