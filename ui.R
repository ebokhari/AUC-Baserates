# ui file
# app for visual demonstration of how the positive and negative predictive values 
# change across fixed sensitivity and specificity values when the base rate changes

shinyUI(
     fluidPage(
          title = 'The AUC and Base Rates',
          
          fluidRow(
               column(12,
                      # message
                      h3('Visual demonstration of the AUC and base rates', align = 'center'),
                      br(), 
                      p('The AUC is independent on base rates meaning that it should remain relatively
                        stable across different, but similar, populations with different base rates
                        of event being predicted or signal being detected. For this app, select the AUC for a 
                        diagnostic test and the base rate of the sample. The output will display two normal distributions
                        for the signal and noise populations and the associated ROC curve. In addition, three 
                        randomly chosen cutscores will be used and a 2 x 2 contingency table constructed from these 
                        scores. The test will be evaluated for clinical efficiency at the two larger cutscores and, if the
                        tests are not clinically efficient, the range for the cost of false negatives to false
                        positives are provided so the test is generalized clinically efficient. Plots to display the 
                        cost, k, of false negatives to false positives for the given PPV and NPV at the two cutscores
                        are provided.')
               )
          ),
          
          fluidRow(
               column(4, offset = 2,
                      # input value of AUC
                      sliderInput('AUC', label = h5('Select AUC (between .5 and 1)'), 
                                  min = .5, max = .99, value = .75, step = .01)
               ),
               
               column(4, offset = 2,
                      # input baserate
                      sliderInput('baserate', label = h5('Select base rate (between 0 and .5)'),
                                  min = 0.01, max = .49, value = .25, step = .01)
               )
          ),
          
          fluidRow(
               column(4,
                      plotOutput('rocPlot'),
                      plotOutput('distPlot')
               ),
               
               column(4,
                      plotOutput('rawPlot'),
                      textOutput('BHcond1'), textOutput('BHcond2')
               ),
               
               column(4,
                      plotOutput('ppvPlot1'),
                      plotOutput('ppvPlot2')
               )
          )
     )
)
