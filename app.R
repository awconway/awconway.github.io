library(shiny)
library(jsonlite)
library(curl)
#currency conversion

currencyrobject=fromJSON("https://openexchangerates.org/api/latest.json?app_id=db370e9558ff4946b1f4ffd919f79a90")

rates=as.data.frame(currencyrobject$rates)


choices=names(rates)


# Define UI for application that draws a plot of the net benefit of FAW
ui <- fluidPage(
  
  # Application title
  titlePanel("Cost-effectiveness of forced air warming"),
  
  # Sidebar with a slider inputs for FAW and laundry costs as well as currency
  sidebarLayout(
    sidebarPanel(
      
      helpText("This webpage will help you decide if using forced air warming to improve thermal comfort for patients in the cardiac catheterisation laboratory is good value for your hospital"),
      sliderInput(inputId = "FAW", label = "Select the cost of one forced air warming blanket at your hospital:",
                  min = 0,
                  max = 20,
                  value = 6, step=0.1, round = FALSE),
      
      sliderInput(inputId = "laundry", label = "Select the cost of laundering one cotton blanket at your hospital:",
                  min = 0,
                  max = 5,
                  value = 2.5, step=0.1, round = FALSE),
      
      selectInput(inputId = "currency", label = "Select your currency code:",
                  choices=c("AUD", "CAD", "EUR", "GBP", "USD")
      )
    ),
    
    # Show a plot of the net benefit
    mainPanel(
      plotOutput(outputId ="netbenefit")
    )
  )
)

# Define server logic required to draw a plot of the net benefit of FAW across a range of WTP
server <- function(input, output) {
  
  output$netbenefit <- renderPlot({
    
    rgbeta <- function(n, mean, var, min = 0, max = 1)
    {
      dmin <- mean - min
      dmax <- max - mean
      
      if (dmin <= 0 || dmax <= 0)
      {
        stop(paste("mean must be between min =", min, "and max =", max)) 
      }
      
      if (var >= dmin * dmax)
      {
        stop(paste("var must be less than (mean - min) * (max - mean) =", dmin * dmax))
      }
      
      # mean and variance of the standard beta distributed variable
      mx <- (mean - min) / (max - min)
      vx <- var / (max - min)^2
      
      # find the corresponding alpha-beta parameterization
      a <- ((1 - mx) / vx - 1 / mx) * mx^2
      b <- a * (1 / mx - 1)
      
      # generate standard beta observations and transform
      x <- rbeta(n, a, b)
      y <- (max - min) * x + min
      
      return(y)
    }
    
    
    #parameter distributions
    
    FAWcosts=input$FAW
    FAWblankets=rgbeta(n=1000,1.6,0.8, min=1, max=10)
    Standardblankets=rgbeta(n=1000,2.2,1.3, min=1, max=10)
    laundrycosts=input$laundry
    FAWtotalcosts=((FAWblankets*laundrycosts)+FAWcosts)*rates[[input$currency]]
    standardcosts=(Standardblankets*laundrycosts)*rates[[input$currency]]
    successwarming=rbeta(n=1000,63,7)
    successstandard=rbeta(n=1000,51,18)
    
    wtpvector=(0:100)
    
    #function for calculating incremental net benefit for FAW at WTP thresholds ranging from $0-100
    
    
    functionincremental=function(x){
      
      
      netbenefitwarming=x*successwarming - FAWtotalcosts
      netbenefitstandard=x*successstandard - standardcosts
      incrementalnb=netbenefitwarming - netbenefitstandard
      
    }
    
    resultincremental=lapply(wtpvector,functionincremental)
    incrementaldf=as.data.frame(resultincremental)
    
    means_incrementaldf=colMeans(incrementaldf)
    
    
    #functions for calculating lower and upper credibility intervals across the range of WTP thresholds
    
    lowcredfunction=function(x){quantile(x, 0.025)}
    
    lowercred=lapply(incrementaldf,lowcredfunction)
    
    uppercredfunction=function(x){quantile(x,0.975)}
    
    uppercred=lapply(incrementaldf,uppercredfunction)
    
    #Code for plot of expected incremental benefit
    
    plot(wtpvector, means_incrementaldf, ylim=c(-20,30), xaxs="i",type="l", ylab="Expected incremental benefit", xlab="Willingness to pay threshold", main="Expected incremental benefit and 95% credible intervals")
    abline(h=0)
    polygon(c(wtpvector,rev(wtpvector)),c(lowercred,rev(uppercred)),border = FALSE)
    lines(wtpvector, means_incrementaldf, lwd=2)
    lines(wtpvector, uppercred, col="red", lty=2)
    lines(wtpvector, lowercred, col="red", lty=2)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
