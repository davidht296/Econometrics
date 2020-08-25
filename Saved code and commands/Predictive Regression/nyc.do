// Load data
cd H:\ECON3203\2018\W1
log using "nyc"
insheet using nyc.csv, clear
/* DO NOT EXECUTE THE ENTIRE .DO FILE UNLESS YOU KNOW WHAT EACH LINE MEANS */

// exploratory plot
summarize 
// give summary of the data being used, not very useful, but this will help later

// histogram
histogram price, normal name(price,replace) // histogram with a normal kde 
histogram food, normal name(food,replace)
gr combine price food
graph export "histogram.png" //, replace 
//- put replace at the end there if you want to remove existing file

// scatter plot
scatter price food // scatter y x give scatter plot of y against x


// regress price on food
// regress [dependent var] [independent var]
reg price food
// how to print out result?
outreg2 using "nyc.tex"
log close
// to get fitted value
// predict [name of new var], [what quantity]
predict residual, residual // residual
predict standardized_resid, rstandard // standardized residual
predict stud_resid, rstudent // Studentized residual
predict fitted, xb // fitted value of y
predict h_i, leverage // leverage
// to remove a variable in the environment

drop h_i
// Dignostic plot
// fig 7
scatter residual fitted
// fig 8
scatter residual food, name(f1,replace)
scatter standardized_resid fitted, name(f2,replace)
gr combine f1 f2
graph export "diagnostic1.png" , replace
// fig 9
line standardized_resid case

// fig 10
twoway (scatter stud_resid fitted) // same as scatter stud_resid fitted
// standardized residual should have sd 1(?), so residual = 4 is odd
// fiding the maximum residuals
summarize stud_resid
//
egen maxResid = max(stud_resid)
list if stud_resid >= maxResid

// naive approach
sort stud_resid // sort the data according to stud_resid, ascending order
// note that this will change the data 
list in 168 // view the last row after sort

// fig 11
twoway (scatter stud_resid food)
//fig 12
scatter standardized_resid stud_resid


// Leverage, fig 13
scatter stud_resid h_i
// fig 14
scatter stud_resid fitted, name(p1, replace)
scatter price fitted, name(p2,replace)
scatter stud_resid food, name(p3, replace)
scatter stud_resid h_i, name(p4, replace)
gr combine p1 p2 p3 p4
gr export diagnostic_plot1.png, replace
// normality check of the error
pnorm stud_resid //Figure 15- normal probability plot
// fig 16 - normal quantile plot - normal error should show an almost linear plot
qnorm stud_resid, grid caption(Normal quantile plot of studentized residuals)

// hypothesis testing- 2 side test
gen beta_food = _b[food]
summarize beta_food
gen food_se = _se[food]
gen t_test = beta_food/food_se
// ttail(df,t) show Pr(T>t)
display (ttail(166,t_test) + 1-ttail(166,-t_test)) //  pvalue
// testing the intercept
gen t_test_cons = _b[_cons]/_se[_cons]
display t_test_cons //negative
display (1-ttail(166,t_test_cons))*2 //  pvalue
// or
display (ttail(166,-t_test_cons))*2 
// ttail(df,t) show Pr(T>t)
display ttail(166,t_test) 1- ttail(166,-t_test) //  pvalue
// Refit the regression without the outliers
// regress y x1 x2 [...] if [condition]
reg price food if abs(stud_resid) <3
// get h_i, fitted and stud_resid 
drop stud_resid h_i fitted
predict stud_resid,rstudent
predict fitted, xb
predict h_i, leverage
// make new dignostic plot
scatter stud_resid fitted, name(p1,replace)
scatter price fitted, name(p2,replace)
scatter stud_resid h_i, name(p3,replace)
pnorm stud_resid, name(p4,replace)
graph combine p1 p2 p3 p4
qnorm stud_resid

// obtain prediction interval
// assume normal, create "prediction interval" using standard error of betas
predict stdf, stdf // SE of the forecast 
predict stdp, stdp // SE of the prediction (fitted value)
// the forecast would have higher SE as it include variation in the dependent var
display stdf 
display stdp
// generate CI
gen pred_lower = fitted - 1.96*stdf
gen pred_upper = fitted + 1.96*stdf
// plot with prediction interval
twoway(line fitted food)(line pred_lower food)(line pred_upper food)(scatter price food,msymbol(triangle))

// To show the actual prediction
// The command below display fitted, lower bound and upper bound for obs 1 to 168
list fitted pred_lower pred_upper in 1/168
// Using t distribution
drop pred_lower pred_upper
/* invttail(df, p) give inverse of right-tailed student's t distribution
ie t such that pr(T>t) = p*/
// this means invttail(164,0.975) gives the 25% quantiles of the t with df 164

gen pred_lower = fitted + invttail(164,0.975)*stdf
gen pred_upper = fitted + invttail(164,0.025)*stdf


// F-test
ttest food==0 // F-test
// create log file?
