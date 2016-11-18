#Overall Comments: Very good exercise! I see your understaning in the ggplot from both the code and explanations!


#2. All	Groups:	Create	a	simple	scatter	plot	of	Weight ("Carat") and	Price using	Color
#(the "Color" column	in	the	diamonds	dataframe)	as	a	facet. This	might	the	precursor	for	
#developing	a	model	to predict	price	given	some	characteristic	like	weight. Notice	that	the	
#relationship	is	non-linear. If	we	wanted	to	build	a	linear	model,	this	would	violate	one	of the assumptions
library(ggplot2)
data(diamonds)
ggplot(diamonds,aes(carat,price))+geom_point(aes(color=factor(color)))+labs(title="Diamonds-Weight to Price by Color",x='Weight',y='Price')
#use ggplot functon and aes function to create coordinate system; 
#use geom_point to caputure the color point; use labs function to change the names


#3. All	Groups:	Let's	remove	the	non-linearity and	replot. Think	about	transforming	both	price	
#and	weight.	Given	that	we	are	working	with	dollars	(Price); the	first	transformation	we	
#might	try	is	the	natural	log. Add	the	transformed	columns	to the	dataframe	and	replot.
ggplot(diamonds,aes(log(carat),log(price),color=factor(color)))+geom_point()+
  labs(title="Diamonds-Weight to Price(Linear)",x='Weight',y='Price')
#use ggplot function and aes function to create coordinate system;
#use log function to transform price and weight;
#use labs function to change the names


#4. All	Groups:	Remove	the	linear	trend	(create	a	linear	model	and	use	the	transformed weight	
#on	the	x-axis	and	the	residuals on	the	y-axis).	 If	there	is	a	relationship	that	can	be	modeled	
#with	a	linear	regression model,	then	the	residuals should	be	randomly	distributed.	The	
#resulting	plot	visually indicates	there is	possibly	some	heteroscedasticity	to	manage.
diamonds_lm=lm(log(price)~log(carat),diamonds)
ggplot(diamonds,aes(log(carat),resid(diamonds_lm),color=factor(color)))+geom_point()+
  labs(title="Diamonds-Weight to Price by Color",x='Weight',y='Price Residuals')+
  theme(legend.position="top")
#use lm(linear model) function to remove the linear trend of price and carat
#use ggplot function and aes function to create coordinate system;
#use log function to transform price and weight;
#use labs function to change the names
#use theme function to move the theme to the top


#5. Groups	A,B,C,D:	Use	the	grid package	to	create	the	following	overlay	of	three	plots. The	
#histogram	on	the bottom left	is	a	density	histogram	of	the	price	and	the	histogram	on the
#upper	right is	a	density histogram of	carat. One	way	to	create	this	plot	is	using	viewports.
library(grid)
hist_carat <- ggplot(diamonds,aes(carat,colour=factor(color)))+geom_histogram(aes(y=..density..),binwidth=0.05) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position='none')
hist_price <- ggplot(diamonds,aes(price,fill=factor(color)))+geom_histogram(aes(y=..density..),binwidth=100) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position='none')
#create two histogram for price and carat
diamonds_lm = lm(log(price) ~ log(carat), diamonds) 
ggplot(diamonds,aes(log(carat), resid(diamonds_lm),color=factor(color)))+geom_point()+
  labs(title="Diamonds - Weight to Price by Color",x='Weight',y='Price Residuals')+
  theme(plot.title=element_text(colour='blue'),legend.position='top')
#same as the question 4
a <- viewport(w=0.4,h=0.2,x=0.25,y=0.16)
b <- viewport(w=0.4,h=0.2,x=0.82,y=0.75)
#use viewport to define the postition of the histograms
print(hist_price, vp = a)
print(hist_carat, vp = b)
#print them out

