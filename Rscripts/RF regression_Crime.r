          Random forest regression for crime data
By:       Daniel Carr
File:     Random forest crime data.r 

Topics:   

1. Fitting a random forest regression model
1.1 Model parameters (function arguments)
1.2 A regression model
1.3 A power transformation of the crime rate.   
    
2. Case influence issues in linear regression
   are greatly reduced in the random forests

3. Predictor variable importance and selection 
3.1 Reduced model 1
3.2 A seven variable model
3.3 Another try with seven variables
3.4 A six variable model
3.5 Hexbin scatterplot matrix with smoothes
3.6 Assignment: Find a different six
    variable model
3.6 Looking at variable pairs

4.  Case proximity and variable importance for
    multidimensional scaling, clustering,
    and graphs (vertex and edge)    
4.1 Multidimenisonal scaling
4.2 Clustering comment
4.3 Side note: graphs and the Fiedler projection 
4.4 Proximity calculation examples
4.5 Proximity based case distance plots
4.6 Clustering using many MDS coordinates
4.6 Comments on constructing predictor variables

5.  Local variable importance

6.  Loose ends: new data predicted values 

Due:      

3.  Variable importance plot crimeRf
3.1 Variable importance plot
    Pairs plot
3.4 Variable importance plot
3.5 Hexbin scatterplot matrix with smoothes
3.6 Find different 6 variable model 
 
4.5 One 2d proximity-base MDS scatterplot
    your choice
    ONE 3d proximity-based MDS plot

5.  The 3d local variable importance
    based scatterplot             

0. Setup and data



A previous assignment not necessary provided to this class 
omitted many communities with missing data removed some
variables.  The includes some candidate dependent variables.
What remains are 80 predictor variables and violent crimes
rates per 10000.  

## Run  
 
crimeReg <- read.csv(file="crimeReg.csv", header=TRUE, row.names=1)
colnames(crimeReg)

## End

1. Fitting a random forest model 

1.1 Model parameters (function arguments)

## Run

?randomForest

## End

Read at least part of the help file. 

Select Parameters

ntree: 500  
  This will take while to run.
  Some example below use 1000 trees. 

importance: TRUE  
  We want to pick important variables to consider  
 
proximity: FALSE
  With 1901 cases the proximity (distance) matrix
  is pretty big and takes time to calculate.  
  I am more interest in variable selection than clustering
  cases (communities) based on crime rate prediction
  as least at the beginning. 

  If community latitudes and longitudes were at hand
  we could make map to see if the attribute based clusters
  related to geospatial clusters. 

Two more of a few parameters others of interest

mtry:      The number of variables sampled at each node to 
           to use in splitting the cases.  The defaults
           are not necessarily the best choice. It is often
           easy to try a couple of other settings to 
           see if it makes much as a difference.  

strata:    A (factor) variable that is used for stratified sampling.
           We might for example to sample different proportions
           of small, middle and large population communities
           to better match the national proportions. 

1.2 A regression model 
 
## Run

set.seed(4543)  # We many want to repeat the analysis

crimeRf <- randomForest(x = crimeReg[ , 1:80], y=crimeReg[, 81],
          importance=TRUE,  proximity=FALSE, ntree=500,
          keepForest=TRUE)
crimeRf

## End_______________________________________

Call:
 randomForest(x = crimeReg[, 1:80], y = crimeReg[, 81],
 ntree = 500,      importance = TRUE, 
 proximity = FALSE, keepForest = TRUE) 

               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 26

          Mean of squared residuals: 125195.1
                    % Var explained: 66.16

Note that the mean square error reported by
random forests is 125195 which seems huge
but is not that huge when compared to the crime
rate range (max - min) squared which is
4870**2 = 23.7 million. The percent of
variance explained is a scale free so a
better criterion for understanding and
making comparisons. 

1.3 A power transformation of the
    crime rate.   
    
A power transformation of the dependent 
variable can make a difference. If we use crime rate
to the .2 power as in the previous assignment
regression model, the percent variance explained
goes up to .70 as shown below.

# set.seed(4543)  # We many want to repeat the analysis
# crimeRf <- randomForest(x = crimeReg[ , 1:80],
#            y=crimeReg[, 81]**.2,
#           importance=TRUE,  proximity=FALSE, ntree=1000,
#           keepForest=FALSE)
# crimeRf
#
#
# Call:
# randomForest(x = crimeReg[, 1:80], 
#    y = crimeReg[, 81]^0.2, ntree = 1000,
#    importance = TRUE, proximity = FALSE,
#    keepForest = FALSE) 
#               Type of random forest: regression
#                     Number of trees: 1000
# No. of variables tried at each split: 26
#
#          Mean of squared residuals: 0.1466593
#                    % Var explained: 70.05
#
## End

Here we will focus primarily on the
violent crime rate without transformation.
The fit is a little better with the transformation
but the results would be harder to explain.

we seek a smaller number of variables
that provide a decent fitting model.
This usually leads to easier explanations
which can be help in obtaining deeper 
understanding.  

Given the dependent variable is not transformed,
the major message from the models below is that
in terms of fitting the crime data there are many
almost equally good models.

In my biased view, I think strong family 
cohesiveness or stability is related to lower
crime.  There are several variables that are
positively or negatively related to my
notion of family stability and I conjecture
that different subsets of the variables
may served to adjust for family cohesiveness
or stability.  

Other pick other variables may on their
knowledge or bias and produce models that
fit just as well. 

The connections between the collected data and
variable names and between the names and 
associations in our minds are not necessary the
same.   
  
2. Case influence issues in linear regression
   are greatly reduced in the random forests

In random forest regression there is no direct
analogy to linear regression predictor variable
leverage.  In linear regression consider a
positive predictor with values larger than 1
and think of raising this to a power such as 2. 
This will increase the gaps between the
largest values and increase the leverage
the largest value. For example (1, 2, 3) 
is transformed in (1, 4, 9) and the gap between the 
largest two values has gotten larger.  
In random forest regression transformations to
not change the case rank so lead to building
the same tree.

The case influence in regression also
depends on the magnitude of the case residual.
In random forest a case is out-of-bag roughly
1/3 of the time.  Its dependent variable
value does not affect the growth of the 
corresponding trees, only the internal
assessment of mean squared errors. 

For "in-bag" cases building a regression
tree model a case with an extreme dependent
variable value could influence the
node splitting criterion in its path going
down the tree in roughly 2/3 of the  trees.
In a classification tree building a wrong
class label would influence a node split
by shifting case proportions by one case.    
 
3. Predictor variable importance and selection 

As described in class, random forests
assess variable importance. We can
look at a list or a plot of variable
importance and conjecture about simple
models may do almost as well.
 
## Run

imp <- importance(crimeRf)
imp
varImpPlot(crimeRf,cex=.8)

## End

Much research is devoted to study
of factors related to crime.  
Older studies often focused criminals.
More recently studies have addressed the
places where crime occurs. Some places
are crime attractors.   

There are theories with related
concepts and variables. Below
I put labels over small groups
of variables. 
 
Family cohesiveness 
  PctKidsBornNeverMar
  PctFam2Par
  FemalePctDiv
  PcrYoungKids2Par
  MalePctDiv

Race/ethnic origin
  racePctWhite
  racePctblack
  racePctHisp

Poverty
  HousVacant
  PctPopUnderPov
  PctWubAsst
  PercentUnemployed

Population size
  population
  pctUrban
  popDens


I might try one or two variables
in this cases along with a few more.
However, mostly I will pick from
variables that random forest says
are important.  

Remember that predictor variables that have high
rank correlations can partially 
mask the importance of each other.

In assessing variable importance
scrambling the value of one variable
does not  matter as much when nodes
in the trees can use highly correlated
variables to define tree branches. Rank 
correlated variables partially act
as surrogated for each other.  

When variables are removed from the model
the remaining variables will be more 
frequently used in the trees and the model
fit may not be reduced at all. 

3.1 Reduced model 1

This try at variable reduction uses 
union of variables that made the top 20 in
the columns labeled %IncMSE and IncNode Purity.
    
The script below finds the column subscripts needed
to put the two importance matrix columns
descending order and obtains the union of the 20 top
20 subscripts from for each column.  These become 
subscripts to the rownames (variable names) of the 
importance matrix and yields explanatory variable
name to use in the next model.
     
## Run

n <- 15

ord1 <- order(imp[, 1],decreasing=TRUE)
nam1 <- row.names(imp[ord1, ])[1:n]
nam1

ord2 <- order(imp[, 2],decreasing=TRUE)
nam2 <- row.names(imp[ord2,])[1:n]
nam2

varNam <- union(nam1,nam2) 
varNam

# same results below
# commonSubs <- sort(unique(c(ord1,ord2)))


# We can check spearman rho rank correlation.

checkCor <- round( cor(crimeReg[,varNam],
   method="spearman"),2)
checkCor

# A correlation plot could be put here.

## End ____________________

As in the linear regresssion assignment
We could make a list of highly correlated variables
and drop some of these.

## Run______________________________

set.seed(4543)
crimeFocus1 <- randomForest(x = crimeReg[,varNam], y=crimeReg[,81],
  importance=TRUE,  proximity=FALSE, ntree=500, keepForest=FALSE)
crimeFocus1

impF1 <- importance(crimeFocus1)
impF1
varImpPlot(crimeFocus1,cex=.9)

## End_____________________________

Call:
 randomForest(x = crimeReg[, varNam], y = crimeReg[, 81], ntree = 500,
      importance = TRUE, proximity = FALSE, keepForest = FALSE) 
               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 6

          Mean of squared residuals: 125426.4
                    % Var explained: 66.1
 
In terms of variance explained this model is almost the same
The first model %Var explained was 66.16.
This modle started with the same random number seed as before.
The model has fewer variables to select from
at each node, and hence possibly has better variables used
when making splits at nodes.

The scatterplot matrix specified below will take a while
to appear. Scanning the columns reveals three with much
white due to outliers at least on a  big screen.  
The rows and columns for population,
OtherPerCap, and HousVacant have lots of white.  This indicates
these variables have at least one substantial outlier.

## Run
              
varDep <- colnames(crimeReg)[81]
varNamDep <- c(varNam,varDep )

windows(width=9,height=9)
splom(crimeReg[,rev(varNamDep)],
 xlab='',cex=.5,as.matrix=TRUE,
   main=paste("Violent Crime Rate Per 100000",
     "and Explanatory Variables",sep="\n"),
  pscale=0, varname.cex=0.38
)



## End_____________________________

3.2  A seven variable model__________________________   

We will run a model with fewer variables to
see if the % of variance explained is almost as high.

Note that with seven variables the default number
of variables to try in which picking a split at node
is floor(7/3) = 2.  We could set the argument mtry
to the number we want rather than use the default.

## Run_______________________________
ord <- order(impF1[,1],decreasing=TRUE)
varNam2 <- row.names(impF1[ord,])[1:7]
varNam2


varNam2 <- c("racepctblack", "racePctWhite",       
   "pctWPubAsst",        
   "FemalePctDiv", 
   "PctKidsBornNeverMar",   
   "PctPersDenseHous",
   "PctUsePubTrans")     

set.seed(4543)
crimeFocus2 <- randomForest(x = crimeReg[, varNam2], y=crimeReg[,81],
  importance=TRUE, proximity=FALSE, ntree=500, keepForest=FALSE)

crimeFocus2
varImpPlot(crimeFocus2)

## End_________________________________________________

Call:
 randomForest(x = crimeReg[, varNam2], y = crimeReg[, 81],
      ntree = 1000,
      importance = TRUE, proximity = FALSE, keepForest = FALSE) 
               Type of random forest: regression
                     Number of trees: 1000
No. of variables tried at each split: 2

          Mean of squared residuals: 131842.1
                    % Var explained: 64.37

The fit is not quite as good. 

3.3 Another try with seven variables

## Run

varNam3 <- c(
   "racePctWhite",            
   "FemalePctDiv", "PctKidsBornNeverMar",   
   "PctPersDenseHous","HousVacant",
   "PctUsePubTrans", "PctPopUnderPov") 

set.seed(4543)
crimeFocus3 <- randomForest(x = crimeReg[, varNam3], y=crimeReg[,81],
          importance=TRUE, proximity=FALSE, ntree=500, keepForest=FALSE)

crimeFocus3
varImpPlot(crimeFocus3)

## End______________________________

Call:
 randomForest(x = crimeReg[, varNam3],
  y = crimeReg[, 81], ntree = 1000,      
  importance = TRUE, proximity = FALSE, keepForest = FALSE) 
  
             Type of random forest: regression
                     Number of trees: 1000
No. of variables tried at each split: 2

          Mean of squared residuals: 128759.8
                    % Var explained: 65.2


 Mean of squared residuals: 127481.8
 % Var explained: 65.54

The fit in Section 3.1 with 25 variables in more
more difficult to present and discuss.  I had
66.03% of variance explained whick is only about
0.5% higher.  Changing the random number
seed can make a bigger difference than this.        

3.4  A six variable model

## Run

varNam4 <- c(
   "racePctWhite",            
   "FemalePctDiv", "PctKidsBornNeverMar",   
   "PctPersDenseHous",
   "PctUsePubTrans", "PctPopUnderPov") 
 
set.seed(4543)
#set.seed(37)
crimeFocus4 <- randomForest(x = crimeReg[, varNam4], y=crimeReg[,81],
     mtry=2, importance=TRUE, proximity=FALSE, ntree=500, keepForest=FALSE)

crimeFocus4
imp4 <- importance(crimeFocus4)
imp4
varImpPlot(crimeFocus4)

## End____________________________________
  
The result is 
% Var explained: 64.54

Changing the random number seed to 37 yields
% Var explained: 64.64

3.5 Hexbin scatterplot matrix with smoothes

We will put the crime rate first and then the
explanatory variables in decreasing %IncMSE 
importance order

## Run

ord <- order(imp4[,1],decreasing=TRUE)
varNam4Ord <- varNam4[ord]
depNam <- colnames(crimeReg)[81]
varNamPlus <- c(depNam, varNam4Ord)
varNamPlus

labs <- c(
paste("Violent","Crimes",sep="\n"),
paste("%KidBorn","NeverMar",sep="\n"),
paste("Race%","White",sep="\n"),
paste("%Female","Divorced",sep="\n"),
paste("%Use","PubTrans",sep="\n"),
paste("%Pop","Under Pov.",sep="\n"),
paste("%Pers","DensHous",sep="\n")
)

offDiag <- function(x,y,...){
    panel.grid(h=-1,v=-1,...)
    panel.hexbinplot(x,y,xbins=15,...,border=gray(.7),
       trans=function(x)x^.5)
    panel.loess(x , y, ..., degree=1,span=.65,
       lwd=2,col='red')
  }

onDiag <- 
function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col=rgb(.83,.66,1),lwd=2)
    diag.panel.splom(x, ...)
 }
 
windows(width=9,height=9) 
splom(crimeReg[, varNamPlus],as.matrix=TRUE,
  varnames=labs, 
  xlab='',
   main=paste("Violent Crime Rate Per 100000",
     "and Explanatory Variables",sep="\n"),
  pscale=2, varname.cex=0.62,varname.font=2,
   axis.text.cex=0.5, axis.text.col="purple",axis.text.font=2,
  axis.line.tck=.5,
  panel=offDiag,
  diag.panel = onDiag
)

## End

We can quickly see the scatterplot relationship of %KidBornNeverMar
and Race%White in relationships to Violent Crimes in the top row
and to each other as in the panel above Race%White.  We could fit
surface with the two predictors of the domain as shone in the
panel.   
   

3.6 Assignment: Find a different six variable model.

At least one variable should differ the variables shown
in 3.5.  

Try at least two models and pick the one with
a large percent of variance explained. For this model

a)  Report the explanatory variables, the random
    number seed, and the % of variance explained.  

Using the same variables as in a) report the 
% of variance explained after making the following change
and rerunning the script  
b) Change the seed
c) Change the mtry parameter to 3, the kinds of items in a)
d) Raise the power of the dependent variable to 0.2
    and report kinds if item in a)

4. Case proximities, dissimilarity matrices
   multidimensional scaling data clustering
   case clustering and graphs.

The forest of trees case proximities result from
running both in-bag and oob cases down the trees.
The proximity for each pair of cases
is the number of trees in which they
appear together in a leaf node divided
by the number of trees.  

Proximities are numbers in the interval [0 1].
The proximity of a case to itself can be
defined as 1. 

We can transform a proximity matrix into
a dissimilarity matrix.  Previously when we had
a correlation matrix called cor we used
1-abs(cor) as dissimilarity matrix.  Similarly 
we can use 1- proximity_matrix as a
dissimilarity matrix.

4.1 Multidimensional scaling

Earlier we discussed the use of a singular value
decomposition of a cases by variables matrix.
We could use the first left and right 
eigenvectors to sort the case rows and 
variance columns to produce a better
structure matrix as a basic for printing
tables or plotting graphics.  

The description below is written in 
terms of a distance matrix for cases or
variables.  It also applies to a symmetric
dissimilarity matrix.  Dissimilarities
don't have to meet all the formal 
requirements of a distance such as
being consistent with the triangle rule.  

With a case distance matrix we can use
cmdscale() to obtain plotting coordinates
for cases in a chosen dimension. 
 
One dimensional coordinates are useful
for reordering cases so cases similar
to each other cases are close to each
and dissimilar cases are far apart 
other when they are plotted on a line. 
MDS provides a thoughtful comprise when
all the distances cannot be preserved. 

The case ranks can serve as the rows of
data.frame. This helps to organize
tables and graphics based on the data.frame.   

For example the 1D coordinates can order the variables
in a correlation matrix or a parallel
coordinate plot.    
  
Multidimensional scaling into
two dimensions is common and results
are typically shown as a scatterplot.

Result from multidimensional scaling into more
dimensions can be shown in scatterplot
matrices and parallel coordinate plots. 
If the scatterplot matrix is large scagnotics
can help us cherry pick interesting plots
to show.
 
When the number of coordinate matches the
rank of the distance matrix. Distance between
points in that dimension should be a perfect
the distance matrix. 


4.2 Clustering comment

We can use the distance matrix to cluster the
cases. The hclust() function doesn't like large
distance matrices so we don't use it here.


We will use kmean() further below  

4.3 Side note: Graphs and Fiedler projection  

In Stat 763 we will likely create graphs
whose vertices represent cases and whose
edges connect pairs of cases who proximities
are above a chosen threshold.  This
graph can be converted into a graph Laplacian
which is a matrix, say L[i,j]. L[i,i] is the number
edges connecting directly to case i. Off diagonal
elements L[i,j] are -1 if cases i and j share an
edge and 0 otherwise. The sum of each row is 0.
The Fiedler projection uses selected
eigenvectors from the spectral decomposition
of the scaled graph Laplacian to produce
a plot.       

4.4 Proximity calculation examples

The proximity calculation can take time
and produces a large matrix.  For our
1901 case example it is a 1901 x 1901 matrix.
Below we revisit the varNam4 model,
specify the calculation of the proximity
matrix and reduce the number of trees to
500 to speed the calculation.  

Running the script below reveals, that over
93 percent of the off diagonal proximities
matrix are zero.  These pairs of cases
never appeared together in leaf nodes.
 
For regression the default the minimal size
of terminal nodes leaf nodes is 5.  Increasing
this size give cases more opportunities to 
appear together in left nodes. A second
example set the minimum size to 20.   

We will look at multidimensional scaling
results using both. 

I am a little surprise that the scaling
seems to work.  I don't know how big a
matrix R can handle.  Very large
matrices can lead to storage
and numerical accuracy issues.
The accuracy issues are also partly 
related to the matrix content.  

## Run

set.seed(4543)
crimeFocus4a <- randomForest(x = crimeReg[, varNam4],
  y=crimeReg[,81], importance=TRUE,localImp=TRUE,
  proximity=TRUE, ntree=500, keepForest=FALSE)
crimeFocus4a   # % Var explained 64.22            

head(crimeFocus4a$importance)
tmp <- head(crimeFocus4a$localImportance)

prox <- crimeFocus4a$proximity
caseN <- nrow(prox) # 1901 x 1901
table(diag(prox))
pct0Leaf5 <- 100*sum(prox==0)/(caseN*(caseN-1))
pct0Leaf5  # 93.50%

set.seed(4543)
crimeFocus4b <- randomForest(x = crimeReg[, varNam4],
  y=crimeReg[,81], importance=TRUE, localImp=TRUE,
  nodesize=20,
  proximity=TRUE, ntree=500, keepForest=FALSE)

crimeFocus4b  # $ Var explained 64.48

proxLeaf20 <- crimeFocus4b$proximity
pct0Leaf20 <- 100*sum(proxLeaf20>0)/(caseN*(caseN-1))
pct0Leaf20  # 28.5 

## End__________________________

4.5 Proximity-based case distance plots

# Run: Graphics minimum node size = 5

proximityDist <- 1-crimeFocus4a$proximity
cases3dLeaf5 <- cmdscale(proximityDist,k=3)
head(cases3dLeaf5)

windows()
rval <- range(cases3dLeaf5[,1:2])
plot(cases3dLeaf5[,1],cases3dLeaf5[,2],pty="s",
     xlim=rval,ylim=rval,
     main="Communities: Leaf Size = 5, Proximity MDS")

windows()
rval <- range(cases3dLeaf5[,2:3])
plot(cases3dLeaf5[,2],cases3dLeaf5[,3],pty="s",
     xlim=rval,ylim=rval,
     main="Communities: Leaf Size = 5, Proximity MDS")

library(rgl)

open3d(FOV=0)
aspect3d(x=c(1,1,1))
bg3d(color=c("white","black"))
plot3d(cases3dLeaf5,radius=.004,col="red",type='s',
      main="Minimum Leaf Size = 5")
## End____________________________________________

Left click and drag a corner of the rgl plot to resize it.
Left click in the plot and drag to rotate the plot.
If you want right click in the plot and drag to zoom
   in or out
After finding a view you like make a png file using 
the line below.

## Run
rgl.snapshot("CrimeProx5.png")
## End____________________________________________

The amount of structure in the 3D plot is amazing.  
The 3D plot clarifies structure present in the two 2D plots.  
A quick verbal description of the structure, perhaps
triggered by seeing the next 3D plot, is that it looks
like three triangles with the middle triangle sharing
two edges.

## Run 

proximityDist <- 1-crimeFocus4b$proximity
cases3dLeaf20 <- cmdscale(proximityDist,k=3)
head(cases3dLeaf20)


## Run:  Graphics minimum node size = 20
open3d(FOV=3)
aspect3d(x=c(1,1,1))
bg3d(color=c("white","black"))
plot3d(cases3dLeaf20,radius=.007,col="red",type='s',
      main="Minimum Leaf Size = 20")
## End

Enlarge the plot, rotate it and make a .png file. 

## Run
rgl.snapshot("CrimeProx20.png")
## End

The three plane structure leads me to
speculate that three sets of communities
are being modeled by different variables
whose key variation is two dimensional. 
Maybe the three sets different in 
population size or perhaps
in violent crime rates. 

Some more thinking and graphics 
may provide more clues about the
structure. 

4.6 Clustering using many MDS coordinates

The plan is to cluster the cases in order
to focus on subsets of the data. We can
learn by comparing and contrasting subsets.

We can use cluster membership to color the
cases in 3D MSD plot above or in other plots
cases that may include community locations
in a map.  
 
There are many ways of clustering. We
have a dissimilarity matrix for cases
and could use hierarchical and other
clusters methods based on such a matrix.
The matrix is getting large. 

Here we use  MDS use 100 variable and
use k-means to create 10 clusters.  
The choices of 100 and 10 are ad hoc
and could be consider more carefully/

Goal here is to just give it try.  
We are climb out on limb that my
already be cracked, but its interesting

1) Enclosed in the folder are reference
that address bias in the assessment
of variable importance.  Our choice
of variables did not takes this into
account.  )We are still free to make choice.)

2) The assessment of case proximity may not
be very good. More trees can help. There
are still some issues.  

3) Increasing the leaf node size to reduce the
sparsity in a questionable idea.   

4) The results many not generalize to 
   much larger data sets.  At some size
   SVD breaks down.   

## Run

proximityDist <- 1-crimeFocus4b$proximity
cases4dLeaf20 <- cmdscale(proximityDist,k=4,eig=TRUE)
head(cases4dLeaf20)
cumsum(cases4dLeaf20$eig)/sum(cases4dLeaf20$eig)

## End

Values large should be wrong.   
The first 100 eigenvectors get us to 67.8% 
We are missing part of the variability the
small increment may be more reflectiive of
noise than structure.   

## Run

cases100dLeaf20 <- cmdscale(proximityDist,k=100,eig=TRUE)
classes <- kmeans(cases100dLeaf20$points,centers=10)

open3d(FOV=0)
aspect3d(x=c(1,1,1))
bg3d(color=c("white","black"))
plot3d(cases3dLeaf20,radius=.007,col=rainbow(10)[classes$cluster],
      type='s',
      main="Minimum Leaf Size = 20")

## End

uster membership. 

See the local variable importance
example below see the method
applied in that context.  

4.7 Comments on constructing explanatory variables

Dr. Sutton has much experience with tree models
including random forests.  He comments that
constructing explanatory variables is often
helpful is developing better models.   

When variables are correlated we can use
the first few principle coordinates computed
from the variables to produce new linearly 
independent variables to use instead of
the original variables.

The class will address semi-parametric 
principal components which may work quite
well. 

Nonlinear transformation of explanatory
variables can lead to a better model. 
Sometimes a thoughtfully chosen ratio
(or product) makes sense and really 
improves the fit.

We still have not considered omitting
cases that seem to have outlier values 


5. Local variable importance______________________________ 

More exploratory (and questionable) ideas

The script producing the CrimeFocus4a and 4b models,
specified calculating both global variable importance
and local (individual community) variable importance.  
Below we access local variable importance matrix.

## Run

impDat4a <- t(crimeFocus4a$localImportance)
head(impDat4a)

## End

My understanding is that local importance for the
ith case and jth variable is the average value over the
trees in which the ith case was oob. I think
the value for each such tree is squared error for
the ith case when the jth variable values were scramble
minus
the mean square error for all cases when the
jth variable values were not scrambled.  

The violent crime rate per 100,000 ranges 6.6 to
4877.06 some squared errors could be very large. 
Compute a differece can lead to negative values. 
In some cases scrambling the jth variable can lead
to a better fit.    

## End__________________________
 
Some variables are important for explaining
crime for the whole data set but some subsets of
communities other variable can be more important
for modeling the variability in crime rates.

Maps can help us think geospatially about the
crime dynamics. The dynamics can be different in
different parts of a city, parts of a state
or in different parts of the US. 

Graphics can help us think in variable space about
about in crime dynamics.  (Ideally we think in terms
of variables (attributes), space and time.)
  
First we will used kmeans() to cluster communities based on local
variable importance. Below I chose 3 clusters
hoping there might be some correspondence
between the clusters and the triangle sections in the
proximity distance 3d point plot.  

## Run

clus4a <- kmeans(impDat4a,3,nstart=25)
clus4a$centers
table(clus4a$cluster) # $clusters 

##

Note the clus4a is list with components such
as $centers that has the cluster centroids
and $cluster with tells the cluster to which
the community belongs.

Rather than use local importance matrix
to obtain case distances and then cmdscale() to obtain
plotting coordinates, this time we will use a
singular value decomposition to approximate
local importance matrix. From this we will use
the first three left eigenvectors to make a 3-D
scatterplot the communities.  Also we use the cluster
memberships above to assign colors.

## Run
# impDat4a obtained above
head(impDat4a)
xyzCrime4a <- svd(impDat4a)$u[,1:3]
head(xyzCrime4a)

open3d()
aspect3d(x=c(1,1,1))
bg3d(color=c("white","black"))
par3d(FOV=3)
plot3d(xyzCrime4a,radius=.006,
    col=c("green","red","blue")[clus4a$cluster],
    type='s',
   main="Minimum Leaf Size = 5",
   xlab="x",ylab="y",zlab="z")

## End___________________________

rgl.snapshot("CrimeProx3Color.png")

The clusters are consistent with the plot.  We could have
specified more clusters.  We could use the colors in the
case distance maps and see what happens  

6.  Loose end: Predicted values

This are more than enough ideas and details for one assignment. 
However just a few more comments.

Given a random forest model object and a new data set it is
easy to obtain the predicted values. To see the R documentation
use 
?predict.randomForest   

Suppose we had used the crimeRate**.2 power transformation to get
a better model. Then we would apply the inverse transformation
to predicted values:  predictedCrimeRate**5.  

7.  A quick comparison with results of
    another model by scaling the random
    forest residual


   


