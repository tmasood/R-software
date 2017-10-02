#
# ###################################
#
# Forecasting Product Returns
# Time series Distributed Lags Model
#
# ##################################
#
#

# 18 months of production
ProdWks = 78
runs = ProdWks
totalprds = runs

# Low mean sales volume per week
LowSales = 10000

# Medium mean sales volume per week
MedSales = 20000

# High mean sales volume per week
HighSales = 50000

set.seed( 123)
lsdist = rpois(runs, LowSales)

# plot the histogram
# hist(lsdist)

# Probability that a sold unit will return
prob = 2
probdist = rpois(runs, prob)
probdist = (probdist * 0.01)
# print(probdist)

# Number of units returned for each week sales volume
ReturnUnits = floor(lsdist * probdist)
# print(ReturnUnits)

# This value provides a mean of a probability distribution
RetFreqTable = list()
CumRetUnitsPeriod = c()
ProdRetPrd = c()

for (i in 1:(totalprds+runs))
{
  ProdRetPrd[i] = 0 
}

# mean return delay
RetDelay = 16
RetDelayPrd = 1/RetDelay

for (i in 1:runs)
{
    # Distributed Lags Model
    # Return delay is modeled as geometric distribution. With a mean return delay of "RetDelay" periods
    ReturnUnitsPeriod = rgeom(ReturnUnits[i], RetDelayPrd)
    # print(ReturnUnitsPeriod)
    
    for (j in 1:totalprds)
    {
      # number of units returned for each sales period
      NumRet = length(ReturnUnitsPeriod)
      RetCnt = 0
      if (NumRet != 0)
        {
          for (k in 1:NumRet)
          {
            # count number of units returned in each period for each sales period
            if (ReturnUnitsPeriod[k] == (j - 1))
            {
              RetCnt = RetCnt + 1
            }
          }
      }
      # total number of units returned in each period
      ProdRetPrd[(i-1)+j+1] = ProdRetPrd[(i-1)+j+1] + RetCnt
      # print(NumRet)
    }

}
print(ProdRetPrd)
plot(ProdRetPrd, type="l", col="dark red", lwd="2", main="Surface Product Return Forecasting Model", xlab="Number of Weeks (#)", ylab="Surface Product Returns (#)")
abline(v=ProdWks, col="dark green", lwd="2")
text(x=67,y=70,"End of Sales")

