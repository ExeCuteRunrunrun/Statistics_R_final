
# function to count the number of rows by condition
count_rows_by <- function(d,vbcontrol,dptype,acc=NULL) {
  if (is.null(acc)) {
    length(d$verbcontrol[d$verbcontrol==vbcontrol & d$displaytype==dptype])
  }
  else {
    length(d$verbcontrol[d$verbcontrol==vbcontrol & d$displaytype==dptype & d[,ncol(d)]==acc])
  }
}

# function to calculate the percentage of accuracy
calculate_percentage <- function(d,vbcontrol,dptype) {
  percentage <- count_rows_by(d,vbcontrol,dptype,acc=1)/count_rows_by(d,vbcontrol,dptype)
  return(percentage)
}


# similar function to count the nrow of 3 conditions and the percentage of accuracy for df_reg_acc
count_rows_by_reg <- function(d,vbcontrol,dptype,regvalue,acc=NULL) {
  if (is.null(acc)) {
    length(d$verbcontrol[d$verbcontrol==vbcontrol & d$displaytype==dptype & d$regin==regvalue])
  }
  else {
    length(d$verbcontrol[d$verbcontrol==vbcontrol & d$displaytype==dptype & d$regin==regvalue & d[,ncol(d)]==acc])
  }
}

# function to calculate the percentage of accuracy
calculate_percentage_reg <- function(d,vbcontrol,dptype,regvalue) {
  percentage <- count_rows_by_reg(d,vbcontrol,dptype,regvalue,acc=1)/count_rows_by_reg(d,vbcontrol,dptype,regvalue)
  return(percentage)
}


difference_in_accuracy <- function(df,var,varvalue1,varvalue2) {
  accuracy1 <- length(df[[var]][df[[var]]==varvalue1 & df[,ncol(df)]==1])/length(df[[var]][df[[var]]==varvalue1])
  accuracy2 <- length(df[[var]][df[[var]]==varvalue2 & df[,ncol(df)]==1])/length(df[[var]][df[[var]]==varvalue2])
  return(accuracy1-accuracy2)
}

# function to randomize the dataframe by the given variable 
randomize_data <- function(d, var) {
  d[[var]] <- sample(d[[var]], nrow(d))
  return(d)  
}

