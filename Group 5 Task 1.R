
# Read payroll hours file
filepathPay="C:/R_New/data/PayrollHours.csv"
Payrollhours <- read.csv(file=filepathPay, header=TRUE, sep=",")
#Attaching the file
attach(Payrollhours)
#Calculating Mean of a Column
mean(Payrollhours$Hours)
mean(Payrollhours$Hours, trim=1/10)
median(Payrollhours$Hours)

R <-  (Payrollhours$Hours)
mode(R)

#Bar graph
hist(Payrollhours$Hours,col="green", labels = TRUE, border = TRUE, main="Payroll Hours",xlab="Payroll Hours",ylab="Frequency")

#Line
PayHours = (Payrollhours$Hours)
tmp = hist(PayHours, col="#ABCDEF", main="Payroll Hours",xlab="Pay Hours",ylab="Frequency")
lines(c(min(tmp$breaks),tmp$mids,max(tmp$breaks)),c(0,tmp$counts,0),col ="#FF0000", main="Payroll Hours", xlab="Pay Hours", ylab="Frequency",type="b", pch=19)

#By Day
Hz <- c(Payrollhours$Hours)
Vr <- (Payrollhours$Day)

barplot(Hz,names.arg=Vr,xlab="Day",ylab="Pay Hours",col="blue",
        main="By Day Pay Hours",border="red")


# Summary of Payroll hours
summary(Payrollhours$Hours)

IQR(Payrollhours$Hours)

#============================================================

# Read production units file

filepathPro="C:/R_New/data/Production Units.csv"
ProUnits <- read.csv(file=filepathPro, header=TRUE, sep=",")
attach(ProUnits)

#Mean Median
mean(ProUnits$RevenueHours)
mean(ProUnits$RevenueHours, trim=1/10)
median(ProUnits$RevenueHours)

#Mode Function
mode <- function(R)
{
  uniqv <- unique(R)
  uniqv[which.max(tabulate(match(R, uniqv)))]
}

R <-  (ProUnits$RevenueHours)
mode(R)

#Bar graph
hist(ProUnits$RevenueHours,col="green", labels = TRUE, border = TRUE, main="Revenue Hours",xlab="Revenue Hours",ylab="Frequency")

#Line
RevHours = (ProUnits$RevenueHours)
tmp = hist(RevHours, col="#ABCDEF", main="Revenue Hours",xlab="Revenue Hours",ylab="Frequency")
lines(c(min(tmp$breaks),tmp$mids,max(tmp$breaks)),c(0,tmp$counts,0),col ="#FF0000", main="Revenue Hours", xlab="Revenue Hours", ylab="Frequency",type="b", pch=19)

#By Day
Hz <- c(ProUnits$RevenueHours)
Vr <- (ProUnits$Day)

barplot(Hz,names.arg=Vr,xlab="Day",ylab="Revenue Hours",col="blue",
        main="By Day Revenue Hours",border="red")


# Summary of Revenue hours
summary(ProUnits$RevenueHours)

IQR(ProUnits$RevenueHours)

#==========================================================
#Invoicing Hours



# Read production units file

#filepathPro="C:/R_New/data/Production Units.csv"
#ProUnits <- read.csv(file=filepathPro, header=TRUE, sep=",")
#attach(ProUnits)

#Mean Median
mean(ProUnits$InvoicingHours)
mean(ProUnits$InvoicingHours, trim=1/10)
median(ProUnits$InvoicingHours)

#Mode Function
mode <- function(R)
{
  uniqv <- unique(R)
  uniqv[which.max(tabulate(match(R, uniqv)))]
}

R <-  (ProUnits$InvoicingHours)
mode(R)

#Bar graph
hist(ProUnits$InvoicingHours,col="green", labels = TRUE, border = TRUE, main="Invoice Hours",xlab="Invoicing Hours",ylab="Frequency")

#Line
InvHours = (ProUnits$InvoicingHours)
tmp = hist(InvHours, col="#ABCDEF", main="Invoicing Hours",xlab="Invoicing Hours",ylab="Frequency")
lines(c(min(tmp$breaks),tmp$mids,max(tmp$breaks)),c(0,tmp$counts,0),col ="#FF0000", main="Invoicing Hours", xlab="Invoicing Hours", ylab="Frequency",type="b", pch=19)

#By Day
Hz <- c(ProUnits$InvoicingHours)
Vr <- (ProUnits$Day)

barplot(Hz,names.arg=Vr,xlab="Day",ylab="Invoicing Hours",col="blue",
        main="By Day Invoicing Hours",border="red")


# Summary of Invoicing hours
summary(ProUnits$InvoicingHours)

IQR(ProUnits$InvoicingHours)

