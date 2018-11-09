### shiny app to visualize the acceptance-rejection sampling algorithm ###
library(shiny)

# user interface
ui <- fluidPage(
  titlePanel("Acceptance-Rejection Sampling"),
  sidebarLayout(
  sidebarPanel(
    sliderInput("sample_size",
                "Sample size:",
                min = 0,
                max = 10e3,
                value = 1000, step = 50),
    textInput("pdf", "Enter the PDF from which you want to sample:", "1 - abs(x)"),
    tags$h6("For example,enter 1/(2*0.5)*exp(-abs(x)/0.5) to sample from the Laplace distribution with location parameter = 0 and scale = 0.5"),
    verbatimTextOutput("value"),
    tags$b("Below you can select a proposal distribution"),
    tabsetPanel(id = "tabset",
                tabPanel("Uniform",
                         numericInput("Munif", "Blow-up factor (M)", 2),
                         sliderInput("uni_range", "Range of Uniform distribution", min = -10, max = 10, value = c(-1, 1))
                ),
                tabPanel("Gaussian",
                         numericInput("Mnormal", "Blow-up factor", 1),
                         numericInput("mu", "Mean", 0),
                         numericInput("sigma", "Standard Deviation", 1)
                ),
                tabPanel("Gamma",
                         numericInput("Mgamma", "Blow-up factor", 1),
                         numericInput("shape", "shape", 1),
                         numericInput("scale", "scale", 1)
                )
    ),
    
    actionButton("go", "Sample new data")),
  mainPanel(
    tabsetPanel(
      tabPanel("Accept-Reject ratio", plotOutput("plot")), 
      tabPanel("Summary Stats", tableOutput("summary")), 
      tabPanel("Density of sampled values", plotOutput("plot2"))
    ), 
    sliderInput("plotrange", "x-axis range", min = -10, max = 10, value = c(-2, 2), step = 0.1), 
    textInput("filename", "You can save a csv file of the accepted values here:", "enter_file_name_here"),
    downloadButton("downloadData", "Download"))))

# server function 
server <- function(input, output) {
  observeEvent(input$go,{
    accept_reject1 <- function(n) {
      # generate a random sample from a specified distribution using the accept-reject method
      #   n: sample size to be generated
      # Returns:
      #   a numeric vector of size n with sampled values from the triangular distribution 
      accept.x <- rep(NA, n)
      accept.y <- rep(NA, n)
      reject.x <- numeric(1)
      reject.y <- numeric(1)

      while(any(is.na(accept.x))) {
        sum.na <- sum(is.na(accept.x))  
        # count how many sampled x are still needed
        if (input$tabset == "Gaussian") {
          x <- rnorm(sum.na, mean = input$mu, input$sigma)       
          U <- runif(sum.na, min = 0, max = 1)                   
          y <- input$Mnormal*U*dnorm(x,mean = input$mu, sd = input$sigma)
        }
        if (input$tabset == "Uniform") {
          x <- runif(sum.na, min = input$uni_range[1], max = input$uni_range[2])       
          U <- runif(sum.na, min = 0, max = 1)*dunif(x, min = input$uni_range[1], max = input$uni_range[2])                # sample x from g )                 # sample u from U[0,1]
          y <- input$Munif*U
        }
        if (input$tabset == "Gamma") {
          x <- rgamma(sum.na, shape = input$shape, scale = input$scale)           
          U <- runif(sum.na, min = 0, max = 1)*dgamma(x, shape = input$shape, scale = input$scale)
          y <- input$Mgamma*U
        }
        # keep the x and y values for which y <= f(x)
        accept.x[is.na(accept.x)][y <= PDF(x)] <- x[y <= PDF(x)]
        accept.y[is.na(accept.y)][y <= PDF(x)] <- y[y <= PDF(x)]
        # also keep the x and y values for which y > f(x)
        new.x <- x[y > PDF(x)]
        new.y <- y[y > PDF(x)]
        if (length(new.x) > 0) {
          reject.x[(length(reject.x)+1):(length(reject.y)+length(new.y))] <- new.x
          reject.y[(length(reject.y)+1):(length(reject.y)+length(new.y))] <- new.y
        }
        
      }
      return(list(accept = cbind(accept.x, accept.y),
                  reject = cbind(reject.x, reject.y)))
    }
    # create a function of the specified PDF 
    eval(parse(text = paste('PDF <<- function(x)', input$pdf, sep='')))
    whole_sample <- accept_reject1(input$sample_size)
    accept <- whole_sample$accept
    reject <- whole_sample$reject
    # generate plot containing both accepted and rejected values
    output$plot <- renderPlot({
      plot(NA, xlim = input$plotrange,
           ylim = c(0, max(1, max(whole_sample$reject[,2]))),xlab = "", ylab = "",axes = TRUE)
      
      points(reject[-1,1], reject[-1,2], col = "red")
      points(accept[,1],accept[,2], col = "blue")
      })
    
    download.data <- reactive(accept[,1])
    # generate plot of the density of accepted values in the sample
    output$plot2 <- renderPlot({
      plot(density(accept[,1]), main = "Density of sampled values")
    })
    # generate summary table of sample
    output$summary <- renderTable({
      smmry <- c(unclass(summary(accept[,1])),nrow(accept)/nrow(reject))
      names(smmry)[length(smmry)] <- "accept-reject-ratio"
      data.frame("Summary of Sample" = smmry, check.names = FALSE)
    }, rownames = TRUE)
    # option to download data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$filename, ".csv", sep ="")
      },
      content = function(file) {
        write.csv(download.data(), file, row.names = FALSE)
      }
    )
  })
}

shinyApp(ui, server)

