## Install devtools and the aepi package if not previously installed
install.packages("devtools")
library(devtools)
install_github("bbbruce/aepi")

## Access the aepi package
library(aepi)

## The nilton data.frame is part of the package
## - look at the first 15 rows
head(nilton, 15)

## See a listing of the variables
str(nilton)

## View the frequency table with the table function
## nilton$prevhosp is the row variable, nilton$methicse is the column variable
## The dollar sign is one way to access a variable in a dataframe
table(prevhosp = nilton$prevhosp, methicse = nilton$methicse)

## The OR function in the aepi package uses a formula language to
## generate the tables and ORs from a dataset.
## The extra parentheses are an R idiom that prints the object after
## assignments that would generally show nothing
(crude <- OR(methicse ~ prevhosp, nilton))

## The crude object to which you assigned the output of the OR function contains
## more things
crude$crude$table  # two crudes because the object is named crude, and the crude
                   # table and OR are contained within an element named crude within
                   # that object

## You can also pull out the OR as just a number
crude$crude$OR

## Control for AGECAT. The control variable is listed after a | (pipe)
## character
(age.con <- OR(methicse ~ prevhosp | agecat, nilton))

## Again you can extract the pieces
age.con$crude$table
age.con$strata
age.con$strata$`agecat = 0`$table # notice the ` (backticks)
age.con$strata$`agecat = 1`$OR
age.con$adj$OR
age.con$bd$p
age.con$bd$chisq

## Control for SEX
(gender.con <- OR(methicse ~ prevhosp | sex, nilton))

## Control for PREANTBU
(preantbu.con <- OR(methicse ~ prevhosp | preantbu, nilton))

## Control for both AGECAT and PREANTBU
## Control for more variables by adding them to the right of the pipe
(two.con <- OR(methicse ~ prevhosp | agecat + preantbu, nilton))
