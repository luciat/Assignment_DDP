library(shiny)

library(ggplot2)
library(graphics)
library(MASS)
library(car)

# Read the project data
LT <- read.csv("./data/LeadTime.csv", sep=",")

# Subset the project data by the two types of issues
storyLT <- LT[LT$IssueType=="Story",]
defectLT <- LT[LT$IssueType=="Defect",]

# Initialize some variables
vLT <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
qLTNames <- c("30%", "50%", "70%", "85%", "95%")
qLT <- quantile(LT$LeadTime, probs=c(0.3, 0.5, 0.7, 0.85, 0.95))
qStoryLT <- quantile(storyLT$LeadTime, probs=c(0.3, 0.5, 0.7, 0.85, 0.95))
qDefectLT <- quantile(defectLT$LeadTime, probs=c(0.3, 0.5, 0.7, 0.85, 0.95))
tLTNames <- c("Average", "Median", "30%", "50%", "70%", "85%", "95%")

# Define server logic required to draw the agile lead time histogram
shinyServer(function(input, output, session) {
        
        # Expression that generates the histogram plot and that is reactive
        # to input changes.
        output$leadTimeHist <- renderPlot({
                
                # Validate and get input data
                validate(
                        need(input$story|input$defect, "Select work items to show!"),
                        if (input$fill) {
                                need(input$story&input$defect, 
                                     "Select both work items to show by type!")  
                        }
                )
                showStory <- input$story
                showDefect <- input$defect
                selFill <- input$fill
                
                # Plot the histogram and place the percentile lines
                # according to input data
                if (showStory & showDefect) {
                        avg <- mean(LT$LeadTime)
                        md <- median(LT$LeadTime)
                        vQ <- as.vector(qLT)
                        freqLT <- c(0, 5, 10, 15, 20, 25, 30)
                        qTextPos <- 23
                        if (selFill) {
                                g <- ggplot(LT) + labs(x="Lead time", y="Work done", fill="Issue Type") +
                                        geom_histogram(aes(x=LeadTime, fill=IssueType), binwidth=1)
                        } else {
                                g <- ggplot(LT) + labs(x="Lead time", y="Work done") +
                                        geom_histogram(aes(x=LeadTime), color="black", fill="lightgreen", binwidth=1)
                        }
                } else if (showStory) {
                        avg <- mean(storyLT$LeadTime)
                        md <- median(storyLT$LeadTime)
                        vQ <- as.vector(qStoryLT)
                        freqLT <- c(0, 5, 10, 15, 20)
                        qTextPos <- 13
                        g <- ggplot(storyLT) + labs(x="Lead time", y="Story - Work done") +
                                geom_histogram(aes(x=LeadTime), color="black", fill="lightgreen", binwidth=1)
                } else {
                        avg <- mean(defectLT$LeadTime)
                        md <- median(defectLT$LeadTime)
                        vQ <- as.vector(qDefectLT)
                        freqLT <- c(0, 5, 10, 15)
                        qTextPos <- 13
                        g <- ggplot(defectLT) + labs(x="Lead time", y="Defect - Work done") +
                                geom_histogram(aes(x=LeadTime), color="black", fill="lightgreen", binwidth=1)
                }
                g + scale_x_discrete(breaks=vLT, labels=waiver()) +
                        scale_y_discrete(breaks=freqLT, labels=waiver()) +
                        geom_vline(xintercept=vQ[1], color="#2ca25f", size=1, linetype=2) +
                        geom_vline(xintercept=vQ[2], color="green", size=1, linetype=2) +
                        geom_vline(xintercept=vQ[3], color="yellow", size=1, linetype=2) +
                        geom_vline(xintercept=vQ[4], color="orange", size=1, linetype=2) +
                        geom_vline(xintercept=vQ[5], color="red", size=1, linetype=2) +
                        geom_vline(xintercept=avg, color="blue", size=1, linetype=1) +
                        geom_text(label=qLTNames[1], x=vQ[1], y=qTextPos, size=3) +
                        geom_text(label=qLTNames[2], x=vQ[2], y=qTextPos, size=3) +
                        geom_text(label=qLTNames[3], x=vQ[3], y=qTextPos, size=3) +
                        geom_text(label=qLTNames[4], x=vQ[4], y=qTextPos, size=3) +
                        geom_text(label=qLTNames[5], x=vQ[5], y=qTextPos, size=3)
        })
        
        # This expression generates a table with the values of the average,
        # median and the 30%, 50%, 70%, 85% and 95% percentiles
        output$leadTimeTable <- renderTable({
                
                # Validate and get input data
                validate(
                        need(input$story|input$defect, ""),
                        if (input$fill) {
                                need(input$story&input$defect, "")  
                        }
                )
                showStory <- input$story
                showDefect <- input$defect
                
                # Construct table
                if (showStory & showDefect) {
                        avg <- mean(LT$LeadTime)
                        md <- median(LT$LeadTime)
                        vQ <- as.vector(qLT)              
                } else if (showStory) {
                        avg <- mean(storyLT$LeadTime)
                        md <- median(storyLT$LeadTime)
                        vQ <- as.vector(qStoryLT)
                } else {
                        avg <- mean(defectLT$LeadTime)
                        md <- median(defectLT$LeadTime)
                        vQ <- as.vector(qDefectLT)
                }
                tLTValues <- c(avg, md, vQ)
                dfLT <- matrix(tLTValues, nrow=1)
                colnames(dfLT) <- tLTNames
                dfLT
        })
})
