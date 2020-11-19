#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidygraph)
library(reshape2)
library(stats)
library(gridExtra)
library(ggvis)


subset_TPM <- read.csv('TPM_FSK_DHPG_DMSO.csv')
rownames(subset_TPM) <- subset_TPM$X
subset_TPM$X <- NULL


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Title/description
    titlePanel("Expression of Genes under cAMP/PKA and mGluR1/5 Signaling in Mouse Hippocampal Neurons"),
    mainPanel(
        # Side bar description
        h3("Please enter a gene for comparison"),
        #textInput("gene", "Gene:", value = 'Arc'),
        selectInput("gene", "Gene: ",
                    choices = rownames(subset_TPM)),
        fluidRow(
            column(width = 12,
                   plotOutput("plot1"
                   )
            )
        )
    )
)

## Write a function to subset gene of interest

GOI <- function(geney){
    input_gene <- as.data.frame(subset(subset_TPM, rownames(subset_TPM) %in% geney))
    #input_gene <- na.omit(input_gene)
    t_input <- as.data.frame(t(input_gene))
    #colnames(t_input) <- c('TPM')
    t_input$Group <- c(rep('Control',4),rep('DHPG',6),rep('FSK',6))
    t_input <- melt(t_input)
    #input_gene <- unique(input_gene)
    
    return(t_input)
}


# Define server logic required to draw a histogram


server <- function(input, output) {
    
    # Subset data
    selected_gene <- reactive({
        GOI(input$gene)
    })
    
    # Plot data
    output$plot1 <- renderPlot({
        ggplot(data = selected_gene(), aes(x= selected_gene()$Group, y = selected_gene()$value)) + geom_boxplot() +
            theme_classic() + ylab('TPM') + xlab('')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
