#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#setwd("/Users/iopetrid/Desktop/Coursera/Data Science/10_Capstone/")
bg<-readRDS("./bigram.RData")
tg<-readRDS("./trigram.RData")
qd<-readRDS("./quadgram.RData")

library(shiny)
library(stringi)
library(tm)
library(RWeka)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  # Application title
  titlePanel("Word Prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h5("This Shiny application is used to predict the next word based on n-gram algorithm"),
      textInput("text_input", "Please enter your text here",value = ""),
      submitButton('Submit')
    ),
    #Show a plot of the generated distribution
    mainPanel(
      h2("The predicted next word is:"),
      verbatimTextOutput('predicted_word')
    )
  )
))
