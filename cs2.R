cust.acq=read.csv("C:\\Users\\SHAURYA\\Desktop\\R programming\\R case study 2 (Credit card)\\Customer Acqusition.csv")
repayment=read.csv("C:\\Users\\SHAURYA\\Desktop\\R programming\\R case study 2 (Credit card)\\Repayment.csv")
spend=read.csv("C:\\Users\\SHAURYA\\Desktop\\R programming\\R case study 2 (Credit card)\\spend.csv")
View(cust.acq)
View(repayment)
View(spend)
library(dplyr)
library(ggplot2)

str(cust.acq)
# Ques1(a) Replace the values of age which are less than 18 with mean of age values which is 46(approx).
cust.acq$Age<-replace(cust.acq$Age,cust.acq$Age<18,46)
# Ques1(b) Incase the spend amount is more than the limit, replace it with 50% of that of customer's limit.
amount_limit<-merge(cust.acq,repayment)
amount_limit$Amount<-replace(amount_limit$Amount,amount_limit$Amount>amount_limit$Limit,amount_limit$Limit/2)
# Ques1(c) Incase the repayment amount is more than the limit, replace it with the limit.
repayment_limit<-merge(cust.acq,repayment)
repayment_limit$Amount<-replace(repayment_limit$Amount,repayment_limit$Amount>repayment_limit$Limit,repayment_limit$Limit)
View(repayment_limit)
# Ques2 (a) How many distinct customers exist?
cust.acq_spend<-merge(cust.acq,spend)
View(cust.acq_spend)
summary1<-cust.acq_spend%>%group_by(Customer)%>%summarise(Total=n())
View(summary1)
# Ques2 (b) How many distinct categories exist?
summary2<-cust.acq_spend%>%group_by(Segment)%>%summarise(Total=n())
View(summary2)
# Ques2 (c) What is the average monthly spend by customers?
summary3<-cust.acq_spend%>%group_by(Month)%>%summarise(Average=mean(Amount))
View(summary3)
# Ques2 (d) What is the average monthly repayment by customers?
summary4<-spend%>%group_by(Month)%>%summarise(Average=mean(Amount))
View(summary4)
# Ques2 (e) If the monthly rate of interest is 2.9%, What is the profit earned by the banks?
spend_repayment<-merge(spend,repayment)
repayment$Profit<-(repayment$Amount-spend$Amount)*0.029
repayment$Profit<-replace(repayment$Profit,repayment$Profit<0,0)
repayment$date<-dmy(repayment$Month)
repayment$Month_No.<-month(repayment$date)
repayment$Year<-year(repayment$date)
monthly_profit<-repayment%>%group_by(Month_No.)%>%summarise(Profit=sum(Profit))
# Ques2 (f) What are the top 5 product types?
summary6<-spend%>%group_by(Type)%>%summarise(Total=n())%>%arrange(desc(Total))
View(summary6)
# Ques2 (g) Which city has maximum spend?
summary7<-cust.acq_spend%>%group_by(City)%>%summarise(Amountt=sum(Amount))%>%arrange(desc(Amountt))
View(summary7)
# Ques2 (h) Which age group is spending more money?
summary8<-cust.acq_spend%>%group_by(Age)%>%summarise(Amounttt=sum(Amount))%>%arrange(desc(Amounttt))
View(summary8)
# Ques2 (i) Which age group is spending more money?
summary9<-repayment%>%group_by(Customer)%>%summarise(Amountttt=sum(Amount))%>%arrange(desc(Amountttt))
View(summary9)

# Ques3 Calculate the city wise spend on each product on yearly basis?Also include a graphical representation of the same.
library(lubridate)
spend$date<-dmy(spend$Month)
spend$Year<-year(spend$date)

yearly_spent_prod<-cust.acq_spend%>%group_by(City,year,Product)%>%summarise(Amount=sum(Amount))

spent.2.0.0.4<-cust.acq_spend%>%group_by(City,year=2004,Product)%>%summarise(Amount=sum(Amount))
spent.2.0.0.5<-cust.acq_spend%>%group_by(City,year=2005,Product)%>%summarise(Amount=sum(Amount))
spent.2.0.0.6<-cust.acq_spend%>%group_by(City,year=2006,Product)%>%summarise(Amount=sum(Amount))
library(ggplot2)
options(scipen=5)
p1<-ggplot(spent.2.0.0.4, aes(City,Amount))+geom_bar(stat = "identity",aes(fill = Product),position="dodge")
p2<-ggplot(spent.2.0.0.5, aes(City,Amount))+geom_bar(stat = "identity",aes(fill = Product),position="dodge")
p3<-ggplot(spent.2.0.0.6, aes(City,Amount))+geom_bar(stat = "identity",aes(fill = Product),position="dodge")

# Ques4 (a) Graph for monthly comparison of total spends,city wise
spend$Month_No.<-month(spend$date) month
monthly_spend1<-cust.acq_spend%>%group_by(City,month=1)%>%summarise(Total=sum(Amount))
monthly_spend2<-cust.acq_spend%>%group_by(City,month=2)%>%summarise(Total=sum(Amount))
monthly_spend3<-cust.acq_spend%>%group_by(City,month=3)%>%summarise(Total=sum(Amount))
monthly_spend4<-cust.acq_spend%>%group_by(City,month=4)%>%summarise(Total=sum(Amount))
monthly_spend5<-cust.acq_spend%>%group_by(City,month=5)%>%summarise(Total=sum(Amount))
monthly_spend6<-cust.acq_spend%>%group_by(City,month=6)%>%summarise(Total=sum(Amount))
monthly_spend7<-cust.acq_spend%>%group_by(City,month=7)%>%summarise(Total=sum(Amount))
monthly_spend8<-cust.acq_spend%>%group_by(City,month=8)%>%summarise(Total=sum(Amount))
monthly_spend9<-cust.acq_spend%>%group_by(City,month=9)%>%summarise(Total=sum(Amount))
monthly_spend10<-cust.acq_spend%>%group_by(City,month=10)%>%summarise(Total=sum(Amount))
monthly_spend11<-cust.acq_spend%>%group_by(City,month=11)%>%summarise(Total=sum(Amount))
monthly_spend12<-cust.acq_spend%>%group_by(City,month=12)%>%summarise(Total=sum(Amount))

g1<-ggplot(monthly_spend1,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g2<-ggplot(monthly_spend2,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g3<-ggplot(monthly_spend3,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g4<-ggplot(monthly_spend4,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g5<-ggplot(monthly_spend5,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g6<-ggplot(monthly_spend6,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g7<-ggplot(monthly_spend7,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g8<-ggplot(monthly_spend8,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g9<-ggplot(monthly_spend9,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g10<-ggplot(monthly_spend10,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g11<-ggplot(monthly_spend11,aes(City,Total))+geom_bar(stat="identity",position="dodge")
g12<-ggplot(monthly_spend12,aes(City,Total))+geom_bar(stat="identity",position="dodge")

# Ques 4 (b) yearly spend on air tickets
air_ticket_spent<-cust.acq_spend%>%group_by(Type="AIR TICKET",year)%>%summarise(Total=sum(Amount))
air_graph<-ggplot(air_ticket_spent,aes(year,Total))+geom_bar(stat="identity",position="dodge")

# Ques4 (c) monthly spend for each product
monthly_product_spend<-cust.acq_spend%>%group_by(Product,month)%>%summarise(Total=sum(Amount))
graph<-ggplot(monthly_product_spend,aes(month,Total))+geom_bar(stat="identity",aes(fill=Product),position="dodge")

# Ques5

my_function<-function(x){
  a<-x%>%group_by(Product,Year,Customer)%>%summarise(Total=sum(Amount))%>%arrange(desc(Total))
  top10<-head(10,a)
  return(c(a=a,top10=top10))
}
f1<-filter(amount_limit,Year=="2004",Product=="Gold")
f2<-filter(amount_limit,Year=="2005",Product=="Gold")
f3<-filter(amount_limit,Year=="2006",Product=="Gold")
f4<-filter(amount_limit,Year=="2004",Product=="Silver")
f5<-filter(amount_limit,Year=="2005",Product=="Silver")
f6<-filter(amount_limit,Year=="2006",Product=="Silver")
f7<-filter(amount_limit,Year=="2004",Product=="Platinum")
f8<-filter(amount_limit,Year=="2005",Product=="Platinum")
f9<-filter(amount_limit,Year=="2006",Product=="Platinum")
my_function(f1)



View(cust.acq_spend)





