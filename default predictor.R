#________________________________________________________________________________________________-
#Gramaner EDA case study
#________________________________________________________________________________________________-

# Company name: consumer finance company (Lending club)

# Data provided: loan.csv with 39717 rows and 111 columns

# Description: 
# The data contains information about accepted loan applications in one of 
# 3 statuses - "Fully paid", "Current", "Charged Off".
# Here, 
# 1. Fully paid means the loan is closed after full repayment of principal and interest.
# 2. Charged Off means the company closed the loan account after the customer defaulted on the loan.
# Such loans cause the company to incur credit loss.
# The credit loss is the amount of money lost by the lender when the borrower 
#refuses to pay or runs away with the money owed. In other words, borrowers 
#who default cause the largest amount of loss to the lenders.
# 3. Current: Ongoing loans. 

#Objective:
# Identify the driving factors (or driver variables) behind loan default, 
#i.e. the variables which are strong indicators of default    

#________________________________________________________________________________________________-
# Load libraries. Install if not present
#________________________________________________________________________________________________-

if(!require(stringr))
{
  install.packages("stringr")
}

if(!require(dplyr))
{
  install.packages("dplyr")
}

if(!require(ggplot2))
{
  install.packages("ggplot2")
}

if(!require(reshape2)) {
  install.packages("reshape2")
}

if(!require(lubridate))
{
  install.packages("lubridate")
}

library(stringr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

#_________________________________________________________________________________________________
# Load the data set and perform basic checks
#_________________________________________________________________________________________________

loan <- read.csv("loan.csv", header = T, na.strings = c("", "NA", "n/a"))

nrow(loan) #39717
ncol(loan) #111 columns
str(loan)
View(loan)

#________________________________________________________________________________________________________________________________
# Data understanding and cleaning
#________________________________________________________________________________________________________________________________

# Basic cleanup
# Upon reviewing the data visually, it appears that there are columns with all NA values.
# Let us clean up the data by removing such columns

columns <- colnames(loan)
columnsToRemove <- vector("numeric")
for (i in 1:ncol(loan)) {
  if(sum(is.na(loan[,i])) == nrow(loan))
  {
    columnsToRemove <- c(columnsToRemove, i)
  }
}
loan <- loan[, -columnsToRemove]

ncol(loan)

#_________________________________________________________________________________________________
# Understanding the rest of the columns

# Data definitions of all the 57 columns
#_________________________________________________________________________________________________

#1. id: A unique LC assigned ID for the loan listing.
#2. member_id:	A unique LC assigned Id for the borrower member.
#3. loan_amnt - The listed amount of the loan applied for by the borrower. If at some point in time, 
#the credit department reduces the loan amount, then it will be reflected in this value.
#4. funded_amnt - The total amount committed to that loan at that point in time.
#5. funded_amnt_inv - The total amount committed by investors for that loan at that point in time.
#6. term -	The number of payments on the loan. Values are in months and can be either 36 or 60.
#7. int_rate - Interest Rate on the loan
#8. installment - The monthly payment owed by the borrower if the loan originates.
#9. grade - LC assigned loan grade
#10. sub_grade - LC assigned loan subgrade
#11. emp_title - The job title supplied by the Borrower when applying for the loan. Appears to be employer title
#12. emp_length - Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years. 
#13. home_ownership - The home ownership status provided by the borrower during registration. Our values are: RENT, OWN, MORTGAGE, OTHER.
#14. annual_inc - The self-reported annual income provided by the borrower during registration.
#15. verification_status - Indicates if income was verified by LC, not verified, or if the income source was verified
#16. issue_d - The month which the loan was funded
#17. loan_status - Current status of the loan
#18. pymnt_plan - Indicates if a payment plan has been put in place for the loan
#19. url - URL for the LC page with listing data.
#20. desc - Loan description provided by the borrower
#21. purpose - A category provided by the borrower for the loan request. 
#22. title - The loan title provided by the borrower
#23. zip_code - The first 3 numbers of the zip code provided by the borrower in the loan application.
#24. addr_state - The state provided by the borrower in the loan application
#25. dti - A ratio calculated using the borrower's total monthly debt payments on the total debt obligations, 
#excluding mortgage and the requested LC loan, divided by the borrower's self-reported monthly income.
#26. delinq_2yrs - The number of 30+ days past-due incidences of delinquency in the borrower's
#credit file for the past 2 years
#27. earliest_cr_line : The month the borrower's earliest reported credit line was opened
#28. inq_last_6mths: The number of inquiries in past 6 months (excluding auto and mortgage inquiries)
#29. mths_since_last_delinq:	The number of months since the borrower's last delinquency.
#30. mths_since_last_record: The number of months since the last public record.
#31. open_acc:	The number of open credit lines in the borrower's credit file.
#32. pub_rec:	Number of derogatory public records
#33. revol_bal:	Total credit revolving balance
#34. revol_util:	Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
#35. total_acc:	The total number of credit lines currently in the borrower's credit file
#36. initial_list_status:	The initial listing status of the loan. Possible values are - W, F
#37. out_prncp	Remaining outstanding principal for total amount funded
#38. out_prncp_inv:	Remaining outstanding principal for portion of total amount funded by investors
#39. total_pymnt	Payments received to date for total amount funded
#40. total_pymnt_inv	Payments received to date for portion of total amount funded by investors
#41. total_rec_prncp	Principal received to date
#42. total_rec_int	Interest received to date
#43. total_rec_late_fee	Late fees received to date
#44. recoveries -	post charge off gross recovery
#45. collection_recovery_fee	- post charge off collection fee
#46. last_pymnt_d	Last month payment was received
#47. last_pymnt_amnt -	Last total payment amount received
#48. next_pymnt_d	- Next scheduled payment date
#49. last_credit_pull_d	- The most recent month LC pulled credit for this loan
#50. collections_12_mths_ex_med -	Number of collections in 12 months excluding medical collections
#51. policy_code -	"publicly available policy_code=1. new products not publicly available policy_code=2"
#52. application_type -	Indicates whether the loan is an individual application or a joint application with two co-borrowers
#53. acc_now_delinq	- The number of accounts on which the borrower is now delinquent.
#54. chargeoff_within_12_mths -	Number of charge-offs within 12 months
#55. delinq_amnt -	The past-due amount owed for the accounts on which the borrower is now delinquent.
#56. pub_rec_bankruptcies -	Number of public record bankruptcies
#57. tax_liens -	Number of tax liens

# We have also categorized the variables as a) consumer b) Loan and Loan application c) Borrower's credit info/history

#_________________________________________________________________________________________________
# Univariate analysis
#_________________________________________________________________________________________________

#1. loan$id is the primary key
any(is.na(loan$id))
anyDuplicated(loan$id)

#2.member_id
#member id is also not duplicated. So, it can also be unique key
any(is.na(loan$member_id))
anyDuplicated(loan$member_id)

#3. loan_amnt - The listed amount of the loan applied for by the borrower. If at some point in time, 
#the credit department reduces the loan amount, then it will be reflected in this value.
# All entries have a positive loan amount
any(is.na(loan$loan_amnt))
any(loan$loan_amnt <= 0 )
summary(loan$loan_amnt)

#4. funded_amnt - The total amount committed to that loan at that point in time.
any(is.na(loan$funded_amnt))
any(loan$funded_amnt <= 0 )
summary(loan$funded_amnt)

#5. funded_amnt_inv The total amount committed by investors for that loan at that point in time.
# TO DO - come back to this
any(is.na(loan$funded_amnt_inv))
any(loan$funded_amnt_inv <= 0 )
sum(loan$funded_amnt_inv == 0 )

#6. term
any(is.na(loan$term))
summary(loan$term)
# 36 months  60 months 
# 29096      10621 
# Let us remove the string "months" to make this a numeric field
loan$term <- as.numeric(str_replace_all(loan$term, " months", ""))
unique(loan$term)

#7. int_rate
any(is.na(loan$int_rate))

loan$int_rate <- as.numeric(str_replace_all(loan$int_rate, "%", ""))
sum(loan$int_rate > 0)
summary(loan$int_rate)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.42    9.25   11.86   12.02   14.59   24.59 

#8. installment - The monthly payment owed by the borrower if the loan originates.
any(is.na(loan$installment))
sum(loan$installment > 0)
summary(loan$installment)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#15.69  167.02  280.22  324.56  430.78 1305.19 

#9. grade - LC assigned loan grade
any(is.na(loan$grade))
summary(loan$grade)
#A     B     C     D     E     F     G 
#10085 12020  8098  5307  2842  1049   316 

#10. sub_grade - LC assigned loan subgrade
# This fields maps 1-1 to interest rates, and hence duplicates the analysis. May be removed.
any(is.na(loan$sub_grade))
summary(loan$sub_grade)

#11. emp_title - The job title supplied by the Borrower when applying for the loan.*
# Appears to be name of employer

sum(is.na(loan$emp_title))
unique(loan$emp_title)
28821 / 39717
View(loan %>% group_by(emp_title) %>% summarise(count = n()))
#There are many entries which have varying titles but the same company. For example, "24 hour fitness", "24 Hour Fitness, Inc" etc.
# This will involve a mammoth cleaning task. We intend to clean this in the subset for only defaulted loans.
# convert all emp titles to lower case, and trim spaces.

loan$emp_title <- str_trim(str_to_lower(loan$emp_title))

#12. emp_length - Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years. 

sum(is.na(loan$emp_length))
summary(loan$emp_length)
sum(!(is.na(loan$emp_title)) & is.na(loan$emp_length), na.rm = T)
loan[which(!(is.na(loan$emp_title)) & is.na(loan$emp_length)), "emp_title"]
sum(is.na(loan$emp_title) & !is.na(loan$emp_length), na.rm = T)
unique(loan$emp_length)
# There is data where emp title is missing, but emp length available. And also where emp title is not available, but length is present.
# Clean up emp length to replace "< 1 year" by 0, and "10+ years" by 10. Also strip the string " years"
employment_length <- function(x) {
  ret <- str_replace_all(x, "< 1 year", "0")
  ret <- str_replace_all(ret, "10\\+ years", "10")
  ret <- str_replace_all(ret, " years| year", "")
  ret
}

loan <- loan %>% mutate(emp_length = employment_length(emp_length) )


#13. home_ownership - The home ownership status provided by the borrower during registration. Our values are: RENT, OWN, MORTGAGE, OTHER.

any(is.na(loan$home_ownership))
summary(loan$home_ownership)
#MORTGAGE     NONE    OTHER      OWN     RENT 
#17659        3       98     3058    18899 


#14. annual_inc - The self-reported annual income provided by the borrower during registration.
any(is.na(loan$annual_inc) | loan$annual_inc <= 0)
summary(loan$annual_inc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4000   40404   59000   68969   82300 6000000 

#15. verification_status - Indicates if income was verified by LC, not verified, or if the income source was verified
any(is.na(loan$verification_status) | loan$verification_status == "")
summary(loan$verification_status)

#16. issue_d - The month which the loan was funded
any(is.na(loan$issue_d) | loan$issue_d == "")
unique(loan$issue_d)
loan$issue_d <- as.POSIXct(paste("01", loan$issue_d, sep = "-"), format = "%d-%b-%y")
head(sort(loan$issue_d, decreasing = T))
# After converting to POSIX format, all dates are before today. Hence, it is not necessary to treat the century part of year differently.

#17. loan_status - Current status of the loan
any(is.na(loan$loan_status))
summary(loan$loan_status)
sum(loan$loan_status == "Charged Off") * 100 / nrow(loan)
# 14% of the total data is for credit defaults. Hence, the data can be considered good for analysing loan defaults.

#18. pymnt_plan - Indicates if a payment plan has been put in place for the loan
summary(factor(loan$pymnt_plan))
# pymnt_plan is "n" for all entries. This field can be removed since it does not add any value to the analysis.

#19. url - URL for the LC page with listing data.
head(loan$url)
# does not seem to add value to the analysis. Can be removed. 

#20. desc - Loan description provided by the borrower
head(loan$desc)
# May not be needed for analysis. Can be removed.

#21. purpose - A category provided by the borrower for the loan request. 
any(is.na(loan$purpose))
summary(loan$purpose)

#22. title
any(is.na(loan$title))
summary(loan$title)
loan[loan$purpose == 'other', 'title']
# May not be useful for analysis

#There are 'other' entries in purpose, which have a corresponding entry in 'title' that match exactly from one of
# the list of values for purpose. Imputing those values from title field.
loan <- loan %>% mutate(purpose_mutated = ifelse(purpose == 'other' & str_to_lower(title) %in% unique(str_replace_all(purpose, "_", " ")), as.character(str_to_lower(str_replace_all(title, " ", "_"))), as.character(purpose)))

#23. zip_code - The first 3 numbers of the zip code provided by the borrower in the loan application.

any(is.na(loan$zip_code))
# appears to be a 3-digit zip code in the format such as 234xx. Confirming the format.
sum(!str_detect(loan$zip_code, "^[:digit:]{3}xx$"))
# All the rows have the exact same zip code format.

#24. addr_state - The state provided by the borrower in the loan application

any(is.na(loan$addr_state))
summary(loan$addr_state)

#25. dti - A ratio calculated using the borrower's total monthly debt payments on the total debt obligations, 
#excluding mortgage and the requested LC loan, divided by the borrower's self-reported monthly income.

any(is.na(loan$dti))
summary(loan$dti)
sum(loan$dti == 0)

#26. delinq_2yrs - The number of 30+ days past-due incidences of delinquency in the borrower's
#credit file for the past 2 years

any(is.na(loan$delinq_2yrs))
summary(loan$delinq_2yrs)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.1465  0.0000 11.0000 
# Appears that there are many outliers.
sum(loan$delinq_2yrs == 0)
sum(loan$delinq_2yrs > 0)
ggplot(loan, aes(x = 1, y = delinq_2yrs)) + geom_boxplot(outlier.color = "Red")
sum(loan$loan_status == "Charged Off" & loan$delinq_2yrs > 0)
# The box plot clearly shows that all the non-zero values in this field are outliers.
# But there are 691 entries with non-zero value for defaulted loan.
# We will retain for now.


#27. earliest_cr_line : The month the borrower's earliest reported credit line was opened
any(is.na(loan$earliest_cr_line))
unique(loan$earliest_cr_line)
loan$earliest_cr_line <- as.POSIXct(paste("01", loan$earliest_cr_line, sep = "-"), format = "%d-%b-%y")
head(sort(loan$earliest_cr_line, decreasing = T))
#There are entries after convertion to POSIX which are past today. These are not correct since this
#column only contains past dates. Adjusting the century for these rows.
year(loan$earliest_cr_line) <- year(loan$earliest_cr_line) - ifelse(loan$earliest_cr_line > as.POSIXct(Sys.Date()), 100, 0)


#28. inq_last_6mths: The number of inquiries in past 6 months (excluding auto and mortgage inquiries)
#This field may not be useful since inquiries on the credit record does not impact rating. Skipping.

#29. mths_since_last_delinq:	The number of months since the borrower's last delinquency.
summary(loan$mths_since_last_delinq)
sum(loan$mths_since_last_delinq == 0, na.rm = T)

#30. mths_since_last_record: The number of months since the last public record.
summary(loan$mths_since_last_record)
# A Bigger % of NAs.
36931 / nrow(loan)

#31. open_acc:	The number of open credit lines in the borrower's credit file.
summary(loan$open_acc)

#31. pub_rec:	Number of derogatory public records
summary(loan$pub_rec)
sum(loan$pub_rec == 0)
37601/39717
#95% of records have 0 public records. There is very little data for this field to be effective. Ignoring.
ggplot(loan, aes(x = 1, y = pub_rec)) + geom_boxplot(outlier.color = "Red")
# The box plot reveals only outliers.  Retaining field for now
sum(loan$loan_status == "Charged Off" & loan$pub_rec > 0)
#467 rows have non-zero value for loan defaults. Retain field.

#32. revol_bal:	Total credit revolving balance
summary(loan$revol_bal)
sum(loan$revol_bal == 0)

#33. revol_util:	Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
loan$revol_util <- as.numeric(str_replace_all(loan$revol_util, "%", ""))
summary(loan$revol_util)

#34. total_acc:	The total number of credit lines currently in the borrower's credit file
summary(loan$total_acc)

#35. initial_list_status:	The initial listing status of the loan. Possible values are - W, F
summary(loan$initial_list_status)
# This field contains the value 'f' in all rows. Will not add any value to the analysis. Can be removed.

#36. out_prncp	Remaining outstanding principal for total amount funded
summary(loan$out_prncp)
sum(loan$out_prncp == 0)
38577 / 39717
#97% rows are zero valued
ggplot(loan, aes(x=1, y=out_prncp)) + geom_boxplot(outlier.color = "Red")
# The box plot reveals only outliers.  
sum(loan$loan_status == "Charged Off" & loan$out_prncp > 0)
# There is no non-zero value for charged off. This field cannot help the analysis. Removing.

#37. out_prncp_inv:	Remaining outstanding principal for portion of total amount funded by investors
summary(loan$out_prncp_inv)
sum(loan$out_prncp_inv == 0)
# 100% values 0. This field can also be ignored.
ggplot(loan, aes(x=1, y=out_prncp_inv)) + geom_boxplot(outlier.color = "Red")
# The box plot reveals only outliers.  
sum(loan$loan_status == "Charged Off" & loan$out_prncp_inv > 0)
# There is no non-zero value for charged off. This field cannot help the analysis. Removing.

#38. total_pymnt	Payments received to date for total amount funded
summary(loan$total_pymnt)

#39. total_pymnt_inv	Payments received to date for portion of total amount funded by investors
summary(loan$total_pymnt_inv)

#40. total_rec_prncp	Principal received to date
summary(loan$total_rec_prncp)

#41. total_rec_int	Interest received to date
summary(loan$total_rec_int)

#42. total_rec_late_fee	Late fees received to date
summary(loan$total_rec_late_fee)
sum(loan$total_rec_late_fee == 0)
37671 / 39717
#~95% of values = 0. 
sum(loan$loan_status == "Charged Off" & loan$total_rec_late_fee > 0)
# 863 values present for defaulted loans. Retaining.

#43. recoveries	post charge off gross recovery
summary(loan$recoveries)
sum(loan$loan_status == "Charged Off" & loan$recoveries > 0)
# 4218 values present for defaulted loans. Retaining.

#44. collection_recovery_fee	post charge off collection fee
summary(loan$collection_recovery_fee)
sum(loan$loan_status == "Charged Off" & loan$collection_recovery_fee > 0)
# 3782 values present for defaulted loans. Retaining.


#45. last_pymnt_d	Last month payment was received
summary(loan$last_pymnt_d)
loan$last_pymnt_d <- as.POSIXct(paste("01", loan$last_pymnt_d, sep = "-"), format = "%d-%b-%y")
head(sort(loan$last_pymnt_d, decreasing = T))
# After converting to POSIX format, all dates are before today. Hence, it is not necessary to treat the century part of year differently.


#46. last_pymnt_amnt	Last total payment amount received
summary(loan$last_pymnt_amnt)
#47. next_pymnt_d	Next scheduled payment date
summary(loan$next_pymnt_d)
38577 / 39717 #NA percentage
#97% NAs. Remove field

#48. last_credit_pull_d	The most recent month LC pulled credit for this loan
summary(loan$last_credit_pull_d)
#This field will not be relevant for analysis since inquiries do not impact risk. Remove field

#49. collections_12_mths_ex_med:	Number of collections in 12 months excluding medical collections
summary(loan$collections_12_mths_ex_med)
unique(loan$collections_12_mths_ex_med)
loan$purpose[loan$loan_status == "Charged Off" & loan$collection_recovery_fee > 0]
# all 0s and Nas. Remove field

#50. policy_code	"publicly available policy_code=1. new products not publicly available policy_code=2"
summary(as.factor(loan$policy_code))
# All 1s. Remove field

#51. application_type	Indicates whether the loan is an individual application or a joint application with two co-borrowers
summary(loan$application_type)
# All individual application types. Remove field.

#52. acc_now_delinq	The number of accounts on which the borrower is now delinquent.
summary(loan$acc_now_delinq)
# All 0s. Remove field.

#53. chargeoff_within_12_mths	Number of charge-offs within 12 months
summary(loan$chargeoff_within_12_mths)
# All 0s & NAs. Remove field.


#54. delinq_amnt	The past-due amount owed for the accounts on which the borrower is now delinquent.
summary(loan$delinq_amnt)
# All 0s. Remove field.


#55. pub_rec_bankruptcies	Number of public record bankruptcies
summary(loan$pub_rec_bankruptcies)
sum(loan$loan_status == "Charged Off" & loan$pub_rec_bankruptcies > 0, na.rm = T)
# 368 positive entries for loan defaults. Retaining field.

#57. tax_liens	Number of tax liens
summary(loan$tax_liens)
# All 0 & NAs. Remove field.


# Remove unwanted columns as documented in the above section

columnsToRemove <- c("id", "member_id", "sub_grade", "pymnt_plan", "url", "desc", "title",  "inq_last_6mths", "initial_list_status","out_prncp","out_prncp_inv","next_pymnt_d","last_credit_pull_d","collections_12_mths_ex_med","policy_code","application_type","acc_now_delinq","chargeoff_within_12_mths","delinq_amnt","tax_liens")
loan <- loan[loan$loan_status != "Current" , -match(columnsToRemove, names(loan))]
ncol(loan)

# We will only be focusing on the subset of records where loan was defaulted.

loan_default <- loan[loan$loan_status == "Charged Off",]

#______________________________________________________________________________________________________________
#  Segmented Univariate analysis
#_______________________________________________________________________________________________________________

#Objective: Analyse the distributions of each data variable against loan statuses - "Charged Off" and "Fully Paid".
# If there is significant difference in the distribution, we can choose that variable for further analysis. Else,
# we shall ignore those variables.
# Popopulate this vector with variable names selected for further analysis.
factors_to_analyse <- vector('character')

# 1. loan_amount
ggplot(loan, aes(x = loan_status, y = loan_amnt)) + geom_boxplot(outlier.colour = "Red")
# There is not much of a difference in distribution between defaults and fully paid.

#2. funded amount
ggplot(loan, aes(x = loan_status, y = funded_amnt)) + geom_boxplot(outlier.colour = "Red")
# There is not much of a difference in distribution between defaults and fully paid.

#3. funded amount investor
ggplot(loan, aes(x = loan_status, y = funded_amnt_inv)) + geom_boxplot(outlier.colour = "Red")
# There is not much of a difference in distribution between defaults and fully paid.

#4. term

term_status <- loan %>% group_by(loan_status, term) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(term_status, aes(x = factor(loan_status), y = perc, fill = factor(term))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Term by Loan Status") + scale_fill_discrete(name = "Term", labels = c("36 months","60 months"))

#No significant pattern seen

#5. interest rate
ggplot(loan, aes(x = loan_status, y = int_rate)) + geom_boxplot(outlier.colour = "Red")
# It appears that loan defaults start happening at higher interest rates - 11 and above.
ggplot(loan_default, aes(x = int_rate )) + geom_histogram(stat = "bin", binwidth = 0.5, color = "blue")
#For loan defaults, interest rate is almost normally distributed
summary(loan_default$int_rate)
# The mean and median is at about 13.5.
# It is significantly different from the "fully paid" data set
factors_to_analyse <- c(factors_to_analyse, 'int_rate')

#6. installment
ggplot(loan, aes(x = loan_status, y = installment)) + geom_boxplot(outlier.colour = "Red")
# The distribution of data does not show any significant pattern impacting loan defaults.

#7. grade
grade_status <- loan %>% group_by(loan_status, grade) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(grade_status, aes(x = factor(loan_status), y = perc, fill = factor(grade))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Grade by Loan Status") + scale_fill_discrete(name = "Grade")
grade_status <- loan %>% group_by(grade, loan_status) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(grade_status, aes(x = factor(grade), y = perc, fill = factor(loan_status))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Grade", title = "Loan Status by Grade") + scale_fill_discrete(name = "Loan status")
#There is a gradual increase in the percentage of defaulted loans as the grade increases. 
#Each grade maps to a group of interest rates. We have also already factored in interest rates. So, this may be duplicate.
# Still, considering this factor for now.
factors_to_analyse <- c(factors_to_analyse, "grade")

#8. emp_length

emp_length_status <- loan %>% group_by(loan_status, emp_length) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(emp_length_status, aes(x = factor(loan_status), y = perc, fill = factor(emp_length))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Employment length by Loan Status") + scale_fill_discrete(name = "Employment length")
# There is no significant pattern seen here

#9. home_ownership
home_ownership_status <- loan %>% group_by(loan_status, home_ownership) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(home_ownership_status, aes(x = factor(loan_status), y = perc, fill = factor(home_ownership))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Home ownership by Loan Status") + scale_fill_discrete(name = "Home ownership")
#No significant pattern seen

#10. annual_inc
ggplot(loan, aes(x = loan_status, y = log(annual_inc))) + geom_boxplot(outlier.colour = "Red")
# No pattern seen

#ignoring verification status, issue_date, zip since they are not likely to have an impact on status.

#11. addr_state
addr_state_status <- loan %>% group_by(addr_state, loan_status) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(addr_state_status, aes(x = factor(addr_state), y = perc, fill = factor(loan_status))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Loan Status By Address state") + scale_fill_discrete(name = "Loan status")
#It is seen that in terms of percentages, NE has a greater proportion of loan defaults than fully paid and hence stands out.

ggplot(addr_state_status, aes(x = factor(addr_state), y = count, fill = factor(loan_status))) + geom_bar(stat = "identity") + geom_text(size = 2, aes(label = count), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Loan Status By Address state") + scale_fill_discrete(name = "Loan status")
#In terms of count, NE is seen to have a minuscule count and can be disregarded.

#To get a clearer picture, looking at the top 10 states in terms of number of loans.

top_10_states <- head((loan %>% group_by(addr_state) %>% summarise(count = n()) %>% arrange(-count))[,'addr_state'], 10)

addr_state_status_top_10 <- loan %>% filter(addr_state %in% top_10_states$addr_state) %>% group_by(addr_state, loan_status) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(addr_state_status_top_10, aes(x = factor(addr_state), y = perc, fill = factor(loan_status))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Loan Status By Top 10 states") + scale_fill_discrete(name = "Loan status")
# It is seen that the distribution is similar for all the top 10 states.
# Hence, this field can be ignored for further analysis.


#12. dti
ggplot(loan, aes(x = loan_status, y = dti)) + geom_boxplot(outlier.colour = "Red")
#Distribution of dti for the 3 statuses look similar.

#13. delinq_2yrs
ggplot(loan, aes(x = loan_status, y = delinq_2yrs)) + geom_boxplot(outlier.colour = "Red")
#Distribution of delinq_2yrs for the 3 statuses look similar.

#14. earliest_cr_line
ggplot(loan, aes(x = loan_status, y = as.numeric(as.POSIXct(Sys.Date()) - earliest_cr_line))) + geom_boxplot(outlier.colour = "Red")
# distributions looks similar

#15. mths_since_last_delinq
unique(loan$mths_since_last_delinq)
ggplot(loan, aes(x = loan_status, y = mths_since_last_delinq)) + geom_boxplot(outlier.colour = "Red")
# distributions look similar

#16. mths_since_last_record
unique(loan$mths_since_last_record)
ggplot(loan, aes(x = loan_status, y = mths_since_last_record)) + geom_boxplot(outlier.colour = "Red")
#mths_since_last_record is non-zero for all "charged off" rows. Though the median and 75% percentile look
# similar for "charged off" and "Fully paid", inter-quartile range is divergent since min for fully paid is considerably
# lower than that for charged off. This is counter-intuitive.
# Yet, this range does not warrant further analysis.

#17. open_acc
unique(loan$open_acc)
ggplot(loan, aes(x = loan_status, y = open_acc)) + geom_boxplot(outlier.colour = "Red")
# Distributions look the same.

#18. pub_rec
unique(loan$pub_rec)
pub_rec_status <- loan %>% group_by(loan_status, pub_rec) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(pub_rec_status, aes(x = factor(loan_status), y = perc, fill = factor(pub_rec))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Public record by Loan Status") + scale_fill_discrete(name = "Public record")
# The distribution does not show any significant pattern.

#19. revol_bal
unique(loan$revol_bal)
ggplot(loan, aes(x = loan_status, y = revol_bal)) + geom_boxplot(outlier.colour = "Red")
# Distributions look similar

#20. revol_util
unique(loan$revol_util)
ggplot(loan, aes(x = loan_status, y = revol_util, fill = "blue")) + geom_boxplot(outlier.colour = "Red")
# The credit utilized out of total revolving balance for "Charged Off" appear to be greater than those for "Fully Paid".
# Let us consider this field for further analysis.

factors_to_analyse <- c(factors_to_analyse, "revol_util")

#21. total_acc
unique(loan$total_acc)
ggplot(loan, aes(x = loan_status, y = total_acc)) + geom_boxplot(outlier.colour = "Red")
# Distributions look similar

#22. total_pymnt
unique(loan$total_pymnt)
ggplot(loan, aes(x = loan_status, y = total_pymnt)) + geom_boxplot(outlier.colour = "Red")
# The range for "Charged Off" is considerably lower than that of "Fully paid".
# Picking this field for furhter analysis.
factors_to_analyse <- c(factors_to_analyse, "total_pymnt")

#23. total_pymnt_inv
unique(loan$total_pymnt_inv)
ggplot(loan, aes(x = loan_status, y = total_pymnt_inv)) + geom_boxplot(outlier.colour = "Red")
# The range for "Charged Off" is considerably lower than that of "Fully paid".
# But this field is only a portion of total_pymnt
# Hence duplicates the effect of total_pymt. So, ignoring

#24. total_rec_int
unique(loan$total_rec_int)
ggplot(loan, aes(x = loan_status, y = total_rec_int)) + geom_boxplot(outlier.colour = "Red")
# Distributions for "Charged Off" and "Fully Paid" are similar. 


#25. total_rec_late_fee
unique(loan$total_rec_late_fee)
ggplot(loan, aes(x = loan_status, y = total_rec_late_fee)) + geom_boxplot(outlier.colour = "Red")
# No significant difference in the distributions

#26. recoveries
unique(loan$recoveries)
ggplot(loan, aes(x = loan_status, y = recoveries)) + geom_boxplot(outlier.colour = "Red")
# There is significant difference in the range of values as well as outliers. Consider this field for further analysis.
factors_to_analyse <- c(factors_to_analyse, "recoveries")

#27. collection_recovery_fee
unique(loan$collection_recovery_fee)
ggplot(loan, aes(x = loan_status, y = collection_recovery_fee)) + geom_boxplot(outlier.colour = "Red")
# While the IQR itself looks very similar for both "Charged Off" and "Fully paid", there is considerabble higher number
# of outliers for "Charged off". The distribution of collection_recovery_fee looks very close to recoveries.
# Also, logically, the 2 variables should be highly correlated. Hence, we can leave out one.


#28. last_pymnt_d
ggplot(loan, aes(x = loan_status, y = as.numeric(as.POSIXct(Sys.Date()) - last_pymnt_d))) + geom_boxplot(outlier.colour = "Red")
# There is considerable difference in the 2 distributions.Adding this field for analysis.
factors_to_analyse <- c(factors_to_analyse, "last_pymnt_d")


#29. last_pymnt_amnt
ggplot(loan, aes(x = loan_status, y = last_pymnt_amnt)) + geom_boxplot(outlier.colour = "Red")
# There is considerable difference in the 2 distributions.Adding this field for analysis.
factors_to_analyse <- c(factors_to_analyse, "last_pymnt_amnt")

#30. pub_rec_bankruptcies
ggplot(loan, aes(x = loan_status, y = pub_rec_bankruptcies)) + geom_boxplot(outlier.colour = "Red")
#Distributions look alike

#31. Verification status
verification_status_status <- loan %>% group_by(loan_status, verification_status) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(verification_status_status, aes(x = factor(loan_status), y = perc, fill = factor(verification_status))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Verification status by Loan Status") + scale_fill_discrete(name = "Verification status")
#No significant difference in distribution

#32. issue_d
ggplot(loan, aes(x = loan_status, y = as.numeric(as.POSIXct(Sys.Date()) - issue_d))) + geom_boxplot(outlier.colour = "Red")
#No significant difference in distribution

#33. purpose
purpose_status <- loan %>% group_by(loan_status, purpose_mutated) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(purpose_status, aes(x = factor(loan_status), y = perc, fill = factor(purpose_mutated))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Purpose by Loan Status") + scale_fill_discrete(name = "Purpose")
#No significant difference seen in the percentages.

status_purpose <- loan %>% group_by(purpose_mutated,loan_status) %>% summarise(count = n()) %>% mutate(perc = round(count * 100 / sum(count))) 
ggplot(status_purpose, aes(x = factor(purpose_mutated), y = perc, fill = factor(loan_status))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Percent", x = "Loan status", title = "Loan Status By Purpose") + scale_fill_discrete(name = "Loan status")
#No significant difference seen in the percentages, except small business. Look at count to confirm.

ggplot(status_purpose, aes(x = factor(purpose_mutated), y = count, fill = factor(loan_status))) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(count,"")), position = position_stack(vjust = 0.5)) + labs(y = "Count", x = "Loan status", title = "Loan Status By Purpose") + scale_fill_discrete(name = "Loan status")
#No significant difference in distribution

#34. zip
#No specific states have any significant impact on loan defaults. Hence, drilling down on zip is deemed unnecessary.

#35. total_rec_prncp
ggplot(loan, aes(x = loan_status, y = total_rec_prncp)) + geom_boxplot(outlier.colour = "Red")
# The distribution looks significantly lower for charged off as against fully paid. Including for further analysis.
# We assume that the total_pymnt field should tally with the sum of all other payment received fields.
# that is,

sum(loan$total_pymnt == (loan$total_rec_prncp + loan$total_rec_int + loan$total_rec_late_fee + loan$recoveries + loan$collection_recovery_fee))
# 2965. This is a significantly smaller percentage of records for which the tally does not work.
# So, we assume that the given subset of data lacks certain other fields which would account for the total payment accurately.
# Logically, total_rec_prncp is a portion of total_pymnt and would duplicate that analysis.
# Hence, we ignore this total_rec_prncp for further analysis.

#36. emp_title
#There are multiple entries with subtle differences in the titles, but referring to the same company entities.
# Process of cleaning this very advanced and is considered out of scope for this exercise.
# Adding emp_title as it is to factors_to_analyse

factors_to_analyse <- c(factors_to_analyse, "emp_title")

#________________________________________________________________________________________________
# Create derived metrics for the factors selected for analysis 
#_________________________________________________________________________________________________

# There are 2 questions before us.
# 1. Q1: What factors should be considered at the decision point where a loan is accepted or rejected ?
# 2. Q2: For accepted loans, what factors should be considered while deciding funded amount and interest rate ?

# Given the data, we have already ascertained the factors that influence loan defaults in factors_to_analyse
# To this we may add a few other fields in order to derive metrics.


# For Q1. At the time of deciding loan acceptance / rejection,
# we have all customer variables and their credit information / history, as well as the loan amount / other application details.
# From this list, we have 'revol_util' and 'emp_title' in factors_to_analyse
# In addition to this, we can also derive metrics out of loan_amnt, purpose, emp_title

#For Q2. For accepted loans, at the time of deciding funded amount, interest rate,
# we can consider all factors_to_analyse, as well as the following fields to
# derive the 'credit loss' metric - funded_amnt, installment, term


# Filter out other columns, retaining only "factors_to_analyse" and loan_status

factors_to_analyse <- c(factors_to_analyse, "loan_status", "loan_amnt", "purpose_mutated", "funded_amnt", "installment", "term","emp_length", "issue_d")
factors_to_analyse <- c(factors_to_analyse, "total_rec_late_fee", "collection_recovery_fee")

loan_derived <- loan[,factors_to_analyse]
# For derived variable analysis, it is sufficient to only consider defaulted loans
loan_derived_def <- loan_derived[loan_derived$loan_status == 'Charged Off',]
factors_to_analyse


#____________________________________________________________________________________________________________________________
# Question 1: Analyze factors influencing loan acceptance / rejects
#____________________________________________________________________________________________________________________________

# revol_util analysis

View(loan_derived_def)

#Custom function to simplify binning
bin <- function(x, interval, labels = T) {
  if(labels) {
    cut(x, breaks = round((max(x, na.rm = T) - min(x, na.rm = T) )/ interval), right = F)
  } else {
    cut(x, breaks = round((max(x, na.rm = T) - min(x, na.rm = T) )/ interval), right = F, labels = F)
  }
}

#For Q1: Derive variables based on 1. revol_util, 2. emp_title, 3. loan_amnt, 4. purpose

# bins for revol_util
summary(loan_derived_def$revol_util)
interval_ru <- 10
loan_derived_def$revol_util_bins <- bin(loan_derived_def$revol_util, interval_ru)

max(loan_derived_def$revol_util, na.rm=T) -min(loan_derived_def$revol_util, na.rm=T)
99.9/10

#Credit loss is calculated as (installment * term) - (total_pymnt - total_rec_late_fee - collection_recovery_fee)
#Assumption: 1. It is assumed that total_pymnt includes every dollar received into the
# loan account including all installment payments, late fees, recoveries, recovery fee etc.

loan_derived_def <- loan_derived_def %>% mutate(loss = round((installment * term) - (total_pymnt - total_rec_late_fee - collection_recovery_fee)))


# Slice the loss data by revol_util bin to understand which bins have maximum loss.

loss_by_revol_util_bin <- loan_derived_def %>% group_by(revol_util_bins) %>% summarise(total_loss = sum(loss), number_of_defaults = n()) %>% mutate(perc_loss = round(total_loss * 100 / sum(total_loss)), perc_num_def = round(number_of_defaults * 100/ sum(number_of_defaults))) 
ggplot(loss_by_revol_util_bin, aes(x = factor(revol_util_bins), y = total_loss, fill = "blue")) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = total_loss), position = position_stack(vjust = 0.5)) + labs(y = "Loss", x = "Revolving balance utilization rate", title = "Loss by Revolving balance utilization rate")

ggplot(loss_by_revol_util_bin, aes(x = factor(revol_util_bins), y = perc_loss, fill = "blue")) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc_loss,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Loss percentage", x = "Revolving balance utilization rate", title = "Loss % by Revolving balance utilization rate")

ggplot(loss_by_revol_util_bin, aes(x = factor(revol_util_bins), y = number_of_defaults, fill = "blue")) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = number_of_defaults), position = position_stack(vjust = 0.5)) + labs(y = "Number of defaulted loans", x = "Revolving balance utilization rate", title = "number_of_defaults by Revolving balance utilization rate")

ggplot(loss_by_revol_util_bin, aes(x = factor(revol_util_bins), y = perc_num_def, fill = "blue")) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc_num_def,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Number of loan defaults percentage", x = "Revolving balance utilization rate", title = "Number of loan defaults % by Revolving balance utilization rate")

#Drilling into the bin stacks based on purpose.

drilling_by_revol_util_bin <- loan_derived_def %>% filter(revol_util_bins == "[69.9,79.9)") %>% group_by(purpose_mutated) %>% summarise(count = n()) %>% arrange(-count)
drilling_by_revol_util_bin

# Observation 1: 
# revol_util has to be a strong focus at the time of deciding loan acceptance.
# revol_util between 70 and 80 have high probability of loan defaults with highest total credit loss.
# Drilling down on revol_util, probility of loss is highest for loan purpose 'debt consolidation'
#  

# Binning by loan_amnt
summary(loan_derived_def$loan_amnt)
interval_la <- 5000
loan_derived_def$loan_amnt_bins <- bin(loan_derived_def$loan_amnt, interval_la, labels = F)

drilling_by_revol_util_bin <- loan_derived_def %>% filter(revol_util_bins == "[69.9,79.9)") %>% group_by(loan_amnt_bins) %>% summarise(count = n(), sum_la = sum(loan_amnt)) %>% arrange(-count)
drilling_by_revol_util_bin

loan_derived_def$emp_title[loan_derived_def$revol_util_bins == "[69.9,79.9)"]
# Drilldown by loan amount does not reveal anything significant

#emp title
unique(loan_derived_def$emp_title)
View(loan_derived_def %>% group_by(emp_title) %>% summarise(total_loss = sum(loss), num_defaults = n()) %>% mutate(perc_loss = round(total_loss * 100 / sum(total_loss)), perc_num_def = round(num_defaults * 100/ sum(num_defaults) )) %>% arrange(-total_loss))


View(data.frame(loan_derived_def$grade[which(is.na(loan_derived_def$emp_title))]))


drilling_by_emp_title_bin <- loan_derived_def %>% filter(is.na(emp_title)) %>% group_by(purpose_mutated) %>% summarise(count = n()) %>% arrange(-count)
drilling_by_emp_title_bin
# Debt consolidation peaks

drilling_by_emp_title_bin <- loan_derived_def %>% filter(is.na(emp_title)) %>% group_by(loan_amnt_bins) %>% summarise(count = n(), sum_la = sum(loan_amnt)) %>% arrange(-count)
drilling_by_emp_title_bin
# No significant observation on loan amount drill down 

#Observation 2: Those who have not provided an emp title are assumed to be unemployed.
# Data reveals that unemployed cause max credit loss as well as max number of defaults.
# Also, drilling down on purpose, reveals unemployed request debt consolidation loans the highest.
# Hence, LC may considered accepting loan applications of unemployed individuals as highest risk, especially those meant for debt consolidation.
# Also, it appears that LC has not considered providing higher grade for unemployed. This may be re-evaluated.


#_______________________________________________________________________________________________________________________________
# Question 2: After accepting loan, what factors should influence to decide funded amount and interest rates to minimise loss.
#_______________________________________________________________________________________________________________________________

#Interest rate / grade

grade_loss <- loan_derived_def %>% group_by(grade) %>% summarise(total_loss = sum(loss), num_defaults = n()) %>% mutate(perc_loss = round(total_loss * 100 / sum(total_loss)), perc_num_def = round(num_defaults * 100/ sum(num_defaults) )) %>% arrange(-total_loss)
ggplot(grade_loss, aes(x = factor(grade), y = total_loss, fill = "blue")) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = total_loss), position = position_stack(vjust = 0.5)) + labs(y = "Loss", x = "Grade", title = "Loss by Grade")
ggplot(grade_loss, aes(x = factor(grade), y = perc_loss, fill = "blue")) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc_loss,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Loss percentage", x = "Grade", title = "Loss % by grade")
ggplot(grade_loss, aes(x = factor(grade), y = num_defaults, fill = "blue")) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = num_defaults), position = position_stack(vjust = 0.5)) + labs(y = "Number of defaulted loans", x = "Grade", title = "number_of_defaults by Grade")
ggplot(grade_loss, aes(x = factor(grade), y = perc_num_def, fill = "blue")) + geom_bar(stat = "identity") + geom_text(size = 3, aes(label = paste(perc_num_def,"%")), position = position_stack(vjust = 0.5)) + labs(y = "Number of loan defaults percentage", x = "Grade", title = "Number of loan defaults % by Grade")

#Observation 3: Loss percentages is tending to be higher for the grades B,C,D,E
# Volume of loss is greatest for grade C, though B,D, and E are not far off.
# In terms of % of defaults, B has highest risk, followed by C,D and E.
# No concrete recommendation can be made with this observation, except to watch for grade B.

#last_pymnt_d

# Extract month from last_pymnt_d for trend analysis
loan_derived_def$last_pymnt_month <- month(loan_derived_def$last_pymnt_d, label = T, abbr = T)
pymnt_month_loss <- loan_derived_def %>% filter(!is.na(last_pymnt_month)) %>% group_by(last_pymnt_month) %>% summarise(total_loss = sum(loss), num_defaults = n()) %>% mutate(perc_loss = round(total_loss * 100 / sum(total_loss)), perc_num_def = round(num_defaults * 100/ sum(num_defaults) )) %>% arrange(-total_loss)
ggplot(pymnt_month_loss, aes(x = factor(last_pymnt_month), y = perc_loss, group = 1)) + geom_line(stat = "identity")
#Shows a spike in loss for October

# Extract quarter from last_pymnt_d for trend analysis
loan_derived_def$last_pymnt_quarter <- quarter(loan_derived_def$last_pymnt_d)
pymnt_qtr_loss <- loan_derived_def %>% filter(!is.na(last_pymnt_quarter)) %>% group_by(last_pymnt_quarter) %>% summarise(total_loss = sum(loss), num_defaults = n()) %>% mutate(perc_loss = round(total_loss * 100 / sum(total_loss)), perc_num_def = round(num_defaults * 100/ sum(num_defaults) )) %>% arrange(-total_loss)
ggplot(pymnt_qtr_loss, aes(x = factor(last_pymnt_quarter), y = perc_loss, group = 1)) + geom_line(stat = "identity")
#Shows a spike in loss for Q4.

# Extract year from last_pymnt_d for trend analysis
loan_derived_def$last_pymnt_year <- year(loan_derived_def$last_pymnt_d)
pymnt_year_loss <- loan_derived_def %>% filter(!is.na(last_pymnt_year)) %>% group_by(last_pymnt_year) %>% summarise(total_loss = sum(loss), num_defaults = n()) %>% mutate(perc_loss = round(total_loss * 100 / sum(total_loss)), perc_num_def = round(num_defaults * 100/ sum(num_defaults) )) %>% arrange(-total_loss)
ggplot(pymnt_year_loss, aes(x = factor(last_pymnt_year), y = perc_loss, group = 1)) + geom_line(stat = "identity")
#Shows a drastic spike in loan for 2012

ggplot(pymnt_year_loss, aes(x = factor(last_pymnt_year), y = total_loss, group = 1)) + geom_line(stat = "identity")
#Again, data clearly shows greatest and drastic spike of credit loss in 2012.

#Trend percentage number of defaults
ggplot(pymnt_year_loss, aes(x = factor(last_pymnt_year), y = perc_num_def, group = 1)) + geom_line(stat = "identity")

#Trend number of defaults
ggplot(pymnt_year_loss, aes(x = factor(last_pymnt_year), y = num_defaults, group = 1)) + geom_line(stat = "identity")


#Check if percentage of overall loan applications was greatest that year
View(loan %>% group_by(year(last_pymnt_d)) %>% summarise(cnt = n()) %>% arrange(-cnt))
#Not so

#Are the 2012 defaults contributed by some specific loan purpose.
loan_derived_def %>% filter(year(last_pymnt_d)== 2012) %>% group_by(purpose_mutated) %>% summarise(cnt = n()) %>% arrange(-cnt)
#Debt consolition is the highest. In general debt consolidation is highest, so this is not indicative of a problem.

#Do emp titles impact the 2012 loss ?
View(loan_derived_def %>% group_by(year(last_pymnt_d) , emp_title) %>% summarise(cnt = n()) %>% arrange(-cnt))
View(pymnt_year_loss)
# Unemployed is highest. Yet, not strongly indicative of a specific problem.

# Most of the 2012 defaults were issued in which year ?
View(loan %>% filter(year(last_pymnt_d)== 2012 & loan_status == "Charged Off") %>% mutate(age_of_loan = (year(last_pymnt_d) - year(issue_d)) ) %>% group_by(age_of_loan) %>% summarise(cnt = n()) %>% arrange(-cnt))
# Loans issued in 2011 were defaulted the highest.


View(loan %>% filter(loan_status == "Charged Off") %>% group_by(year(issue_d), year(last_pymnt_d)) %>% summarise(n()))
#Loans issued in 2011 and defaulted in 2012 are the highest.

#Were these 2011 issued loans highest in specific loan term ?
View(loan %>% filter(loan_status == "Charged Off" & year(issue_d) == 2011) %>% group_by(term) %>% summarise(cnt = n()) %>% arrange(-cnt))
#Not indicative of a problem

View(loan_derived_def %>% filter(year(last_pymnt_d) == 2012) %>% group_by(last_pymnt_quarter) %>% summarise(total_loss = sum(loss), cnt = n()) %>% arrange(-cnt))
#The 2012 defaults are fairly evenly spread through out the year.

# Observation 3: 
# 2012 has the highest number of defaults as well as total loss.
# 2011 has the maximum of number of loans issued. In fact, it has got doubled from 2010.
# Hence, the probability of defaults spiked for 2012, as a resultant event, and cannot be attributed to any other factor.
# Maximum of the 2012 defaults, were issued in 2011.
# Hence loans issued in 2011 can be scrutinized even further in terms of customer and credit verification.

# Looking at loans issued in 2011
# Slicing the loan defaults data by employee length and income verification status to look at total funded amount and count
View(loan %>% filter(year(issue_d) == 2011 & loan_status == "Charged Off") %>%  group_by(emp_length, verification_status) %>% summarise(cnt = n(), sum_funded_amt = sum(funded_amnt)) %>% arrange(-sum_funded_amt))

# The maximum amount of loans defaulted that were issued in 2011 were for those with employment length >= 10 years
# and who had a verified income.

#Observation 4: Issuing loans to those with longer employment length and verified source of income is not
# indicative of their credit worthiness.
# Potential cause could be that the income verification done had greater failure rate. This can be substantiated
# with a causal analysis investigation on verification.

#recoveries

summary(loan_derived_def$recoveries)
interval_rec <- 5000
loan_derived_def$recovery_bins <- bin(loan_derived_def$recoveries, interval_rec, labels = F)

recovery_bin_count <- loan_derived_def %>% group_by(recovery_bins) %>% summarise(count = n()) %>% arrange(-count)
View(recovery_bin_count)
# This shows that maximum recoveries are between 0 to 5000 dollars.
# Hence potential loss of credit is higher for any loan funded above $5000.
# Any loan amount greater than $5000 have higher risk of non-recovery and should be subject to greater scrutiny.

#last_pymnt_amnt

summary(loan_derived_def$last_pymnt_amnt)

interval_lpa <- 200
loan_derived_def$last_pymnt_bins <- bin(loan_derived_def$last_pymnt_amnt, interval_lpa, labels = F)
summary(loan_derived_def$installment)
loan_derived_def$installment_bin <- bin(loan_derived_def$installment, interval_lpa, labels = F)

View(loan_derived_def %>% group_by(last_pymnt_bins, installment_bin) %>% summarise(count = n()) %>% arrange(-count))
# This is not indicative of any significant trend.

#_____________________________________________________________________________________________________
# Bivariate analysis
#_____________________________________________________________________________________________________

#Based on the derived metrics analysis, choosing the following variables for bivariate combination analysis
#Adding age of loan in days for analysis

loan_derived_def$age_of_loan_days = as.numeric(loan_derived_def$last_pymnt_d - loan_derived_def$issue_d)
factors_for_bivar_analysis <- c("int_rate","revol_util","loss","emp_length","age_of_loan_days","recoveries", "funded_amnt")

loan_bivar <- loan_derived_def[,factors_for_bivar_analysis]
str(loan_bivar)
loan_bivar$emp_length <- as.numeric(loan_bivar$emp_length)
?cor
cor_coeff_matrix <- cor(loan_bivar, method = "pearson", use = "complete.obs" )

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


melted_cormat <- melt(cor_coeff_matrix, na.rm = T)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


melted_for_viewing_perc <- data.frame(melt(round(get_lower_tri(cor_coeff_matrix) * 100), na.rm = T))
melted_for_viewing_perc <- melted_for_viewing_perc[melted_for_viewing_perc$value != 100,]
View(melted_for_viewing_perc)

#Key Observations from bi-variate analysis:
# 1. Combination of variables funded amount, interest rate and loss
#   * Greater funded amounts with high interest rates cause greater loss.
#   * For higher loan amount requests, reduce either one of funded amount or interest rate in order to have lesser impact 
#     on credit loss.It can be recommended to offer lower interest rates for higher funded amounts, 
#     if all other influencing parameters remain neutral."
#
# 2. Combination of variables funded amount, emp length and loss.
#     *Greater funded amount and greater employment length cause greater loss.
#     *Greater employment length should have meant greater credit worthiness. 
#       But, that has not been the case. This means that employment back ground verification process may be reevaluated 
#       to safegaurd against fraudulent candidates. Also, if a strong background checks are lacking, it is highly risky 
#       to provision high funded loans to candidates with high employment length."
# 3.  Combination of varialbes revol_util, age_of_loan_days and loss.
#     * This appears to be a weak combination from the perspective of correlation
#     * The inference is that if revol_util is high, age_of_loan_days can be low.
#     * A lower age_of_loan is indicative of greater loss.
#     * Hence, greater revol_util, can mean greater loss.
#     * A customer with high revol_util is a high risk for credit loss.

#________________________________________________________________________________________________________________________