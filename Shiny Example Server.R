#################################
# Shiny Example Server
# See also Shiny Example UI
#################################

library(datasets)

# Define a server for the Shiny app
function(input, output) {
  
  # Fill in the spot we created for a plot
  output$phonePlot <- renderPlot({
    
      if (input$variable=="Region"){
      # Render a barplot
      barplot(WorldPhones[,input$region]*1000,
              main=input$region,
              ylab="Number of Telephones",
              xlab="Year")
      }
    
      if (input$variable=="Year"){
        # Render a barplot
        barplot(WorldPhones[input$year,]*1000, 
                main=input$year,
                ylab="Number of Telephones",
                xlab="Region")        
      }
    
  })
}


