#1. Print	to	the	console	all	methods	and	attributes	associates	with	a	dataframe.
library("ggplot2") #install and require ggplot2
attributes(diamonds) #use attributes function
methods(class=data.frame) #use methods function

#1. Write	code	to	determine	the	number	of	columns	in	a	dataframe
ncol(diamonds) #use ncol function to count the number of columns


#2. Write	code	to	determine	how	many	rows	are	in	a	dataframe
nrow(diamonds) #use nrow function to count the number of rows
 

#3. Write	code	to	extract	the	column	names	from	a	dataframe	and print	the	names	of	the	columns (one	per	line) to	the	console.
cat(colnames(diamonds),sep="\n") #use cat function


#4. Write	code	to	determine	the	type	of	each	column	(numeric,	factor,	logical, 
#etc.).	Print	the	type	of	each	column	to	the	console.
sapply(diamonds, class) #use sapply function


#5. Write	code	that	will	loop	through	any	dataframe	and	calculate	the	mean	of	
#every	numeric	column. Label	the	output	with	the	name	of	the	column.
sapply(diamonds[sapply(diamonds,is.numeric)],mean,na.rm=TRUE)
#use sapply function and is.numeric function to select the numeric columns
#use na.rm=TRUE delet NAs
#use mean function


#6. Write	code	that	will	loop	through	any	dataframe	and	create	a	frequency	table	
#for	every	factor	column.	Label	the	output	with	the	name	of	the	column.
sapply(diamonds[sapply(diamonds,is.factor)],table)
#use sapply and is.factor function to select factor columns
#use table function


#7. Write	code	that	will	loop	through	any	dataframe	and	determine	the	number	
#of	rows	containing	NA	(missing	value)	in	each	column and the	percentage	of	
#rows containing	an	NA in	any	of	the	columns. HINT:	In	a	single	row,	zero	or	
#more	columns	may	contain	an	NA.	For	the	percentage of	rows	containing NA
#in	any	column,	do	not	double	count	NA	in	rows	that	contain	more	than	one	
#column	with	an	NA. Print	the	results	to	the	console.
apply(sapply(diamonds,is.na),2,sum) #use apply function, 2 means row
temp <- sum(rowSums(is.na(diamonds))>0)/nrow(diamonds)
sum_per <- cat(temp*100,'%',sep='')  #percentage format


#8. Create an	R	function	that	can	accept	any	dataframe	as	a	parameter	and	
#returns	a dataframe	that	contains	each	pair	of	column names	in	the	first	
#column	in	a	single	string	separated	by	a	-,	e.g.	for	the	variables	x	and	y,	you	
#should	form	the	string	"x-y" (HINT: Look	at	the	help	provided	for	the	paste	
#function)	and	their	corresponding	Pearson	correlation	coefficient	in	the	
#second	column.
pearson_coe <- function(dafra){
  dafra=dafra[sapply(dafra,is.numeric)] #select numeric columns
  a=colnames(dafra)#set a as the column names
  pair_names=c() #create an empty list
  pair_coe=c() #create an empty list
  for (i in 1:(length(a)-1)){ #from the first column to the second-last column
    for (j in (i+1):length(a)){ #from the second column to the last column 
      pair_names = c(pair_names,paste(a[i],a[j],sep='-')) #name1+'-'+name2
      pair_coe = c(pair_coe,cor(dafra[i],dafra[j],method="pearson"))#calculate coefficient 
    }
  }
  return (data.frame(pair_names,pair_coe)) #return a new dataframe
}
#e.g.
pearson_coe(diamonds)
