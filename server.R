# server file
# app for visual demonstration of how the positive and negative predictive values 
# change across fixed sensitivity and specificity values when the base rate changes

library(ggplot2)
library(pROC); library(MASS)

shinyServer(
     function(input, output) {    
          
          N = 10000
          
          # mean of signal distribution 
          mu2 <- reactive({
               sqrt(2)*qnorm(input$AUC)
          })
          
          # BR is base rate, inputted by user
          BR <- reactive({
               input$baserate
          })
          
          # true positive values
          tpr <- reactive({
               1 - pnorm(seq(-5, 5 + mu2(), .01), mu2())
          })
          
          # false positive values
          fpr <- reactive({
               1 - pnorm(seq(-5, 5 + mu2(), .01))
          })
          
          # random sample of points from ROC curve
          s <- reactive({
               sort(sample(seq(10, nrow(unique(round(cbind(tpr(), fpr()),2))) - 10, 7), 3), decreasing = T)
          })
          
          # data frame of sensitivity and specificity values
          dfR <- reactive({
               return(data.frame(tpr = tpr(), fpr = fpr()))     
          }) 
          
          #reactive dataset
          # dataset
          D <- reactive({
               sens = c(unique(round(dfR(), 2))[s(),1][1], diff(c(unique(round(dfR(), 2))[s(),1], 1)))
               spec = c(unique(round(dfR(), 2))[s(),2][1], diff(c(unique(round(dfR(), 2))[s(),2], 1)))          

               return(data.frame(V = round(N*BR()*sens), NV = round(N*(1-BR())*spec)))
          })

          # create plot of ROC curve
          output$rocPlot <- renderPlot({
               
               # plot ROC curve
               rocPlot = ggplot(dfR(), aes(x = fpr, y = tpr)) + geom_line(size = 2) +
                    geom_ribbon(aes(x = fpr, ymax = tpr), ymin = 0, alpha = .5) + 
                    scale_x_continuous(name = 'False Postive Rate (1 - Specificity)') +
                    scale_y_continuous(name = 'True Postive Rate (Sensitvity)') +
                    theme(axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17)) +
                    annotate('text', label = paste('AUC =', input$AUC), x = .6, y = .25, 
                             size = 8, colour = 'white') +
                    geom_point(data = data.frame(x = seq(0, 1, .01), y = seq(0, 1, .01)), 
                               aes(x = x, y = y), type = 2, size = 1) +
                    geom_point(data = dfR()[unlist(lapply(unique(round(dfR(), 2))[s(),1], 
                                                        function(x) which(round(dfR(), 2)[,1] %in% x)[1])),]
                               , aes(x = fpr, y = tpr), size = 5, color = 'red')
               
               print(rocPlot)
               
          })
          
          # create plot of signal and noise distributions
          output$distPlot <- renderPlot({ 
               
               # data frame with densities
               dfN = data.frame(x = seq(-5, 5 + mu2(), .01), pop1 = dnorm(seq(-5, 5 + mu2(), .01)),
                                pop2 = dnorm(seq(-5, 5 + mu2(), .01), mu2()))
               
               # plot densities
               distPlot = ggplot(dfN) + geom_line(aes(x = x, y = pop1), col = 'red', size = 2) +
                    geom_line(aes(x = x, y = pop2), col = 'blue', size = 2) + 
                    scale_x_continuous(name = expression(x), breaks = c(0, mu2()), 
                                       labels = c(expression(mu[1]), expression(mu[2])),
                                       limits = c(-5, 5 + mu2())) +
                    scale_y_continuous(name = expression(Phi(x))) + 
                    theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
                          axis.text.x  = element_text(face = 'bold', size = 20))
               
               print(distPlot)
          })
     
          output$rawPlot <- renderPlot({
               
               par(mfrow = c(2,2), mar = c(3,2,2,.5))
               
               #Plot 1: Raw data, cutscore B_4
               plot(0:1, 0:1, type = 'n', ylim = c(0,1), xlim = c(0,1), axes = F, ylab = '', xlab = '')
               abline(h = .9); abline(h = .1); abline(h = .7, col = 'red', lwd = 2); abline(v = .3)
               text(.14, .97, expression(Cutscore), cex = 1)
               text(.475, .97, expression(A), cex = 1)
               text(.825, .97, expression(bar(A)), cex = 1)
               text(.14, .81, expression(B[4]), cex = 1)
               text(.14, .61, expression(B[3]), cex = 1)
               text(.14, .41, expression(B[2]), cex = 1)
               text(.14, .21, expression(B[1]), cex = 1)
               text(.475, .81, D()[1,1], cex = 1)
               text(.475, .61, D()[2,1], cex = 1)
               text(.475, .41, D()[3,1], cex = 1)
               text(.475, .21, D()[4,1], cex = 1)
               text(.825, .81, D()[1,2], cex = 1)
               text(.825, .61, D()[2,2], cex = 1)
               text(.825, .41, D()[3,2], cex = 1)
               text(.825, .21, D()[4,1], cex = 1)
               text(.14, .03, expression(Totals), cex = 1)
               text(.475, .03, colSums(D())[1], cex = 1)
               text(.825, .03, colSums(D())[2], cex = 1)
               
               #Plot 2: Raw data, cutscore B_3
               plot(0:1, 0:1, type = 'n', ylim = c(0,1), xlim = c(0,1), axes = F, ylab = '', xlab = '')
               abline(h = .9); abline(h = .1); abline(h = .5, col = 'red', lwd = 2); abline(v = .3)
               text(.14, .97, expression(Cutscore), cex = 1)
               text(.475, .97, expression(A), cex = 1)
               text(.825, .97, expression(bar(A)), cex = 1)
               text(.14, .81, expression(B[4]), cex = 1)
               text(.14, .61, expression(B[3]), cex = 1)
               text(.14, .41, expression(B[2]), cex = 1)
               text(.14, .21, expression(B[1]), cex = 1)
               text(.475, .81, D()[1,1], cex = 1)
               text(.475, .61, D()[2,1], cex = 1)
               text(.475, .41, D()[3,1], cex = 1)
               text(.475, .21, D()[4,1], cex = 1)
               text(.825, .81, D()[1,2], cex = 1)
               text(.825, .61, D()[2,2], cex = 1)
               text(.825, .41, D()[3,2], cex = 1)
               text(.825, .21, D()[4,1], cex = 1)
               text(.14, .03, 'Base Rate', cex = 1)
               text(.475, .03, sprintf("%.2f",sum(D()[1:4,1])/sum(D())), cex = 1)
               text(.825, .03, sprintf("%.2f",sum(D()[1:4,2])/sum(D())), cex = 1)
               
               labelNames = c('Event', 'NonEvent')
               
               #Plot 3: PPV histogram
               Y = rep(1:2, D()[1,])
               cols = c(ifelse(sum(Y==1)/length(Y) >= .5, cbind(81), cbind(103)),
                        ifelse(sum(Y==1)/length(Y) >= .5, cbind(558), cbind(552)))
               truehist(Y, col = colors()[cols], axes = F, breaks = c(0,1,2), ylim = c(0,1),
                        main = expression('Proportion correct:'~B[4]))
               abline(h = .5, lty = 2)
               axis(side = 1, at = c(.5,1.5), labels = labelNames)
               axis(side=2, at = seq(0, 1, .2))
               
               X = rep(1:2, colSums(D()[1:2,]))
               cols = c(ifelse(sum(X==1)/length(X) >= .5, cbind(81), cbind(103)),
                        ifelse(sum(X==1)/length(X) >= .5, cbind(558), cbind(552)))
               truehist(X, col = colors()[cols], axes = F, breaks = c(0,1,2), ylim = c(0,1),
                        main = expression('Proportion correct:'~B[3]))
               abline(h = .5, lty = 2)
               axis(side = 1, at = c(.5,1.5), labels = labelNames)
               axis(side=2, at = seq(0, 1, .2))
               
          })
          
          # cutscore 4
          output$BHcond1 <- renderText({
               
               # cutscore B4
               BH4 = D()[1,1] > D()[1,2]
               if (!BH4) {
                    kLB4 = D()[1,2]/D()[1,1]
                    kUB4 = sum(D()[2:4,2])/sum(D()[2:4,1])
               }
               
               BHcond1 = ifelse(BH4, paste('Using a cutscore of B4 the test is clinically efficient.'),
                                paste0('The test is not clinically efficient using a cutscore of B4.',
                                       'For the test to be generalized clinically efficient, the cost of false negatives ',
                                       'to false positives would need to be at least ', round(kLB4, 2), 
                                       ' and no more than ', round(kUB4, 2), '.'))
               return(BHcond1)
               
          })
          
          # cutscore 3
          output$BHcond2 <- renderText({
               
               
               # cutscore B3
               BH3 =  sum(D()[1:2,1]) > sum(D()[1:2,2])
               if (!BH3) {
                    kLB3 = sum(D()[1:2,2])/sum(D()[1:2,1])
                    kUB3 = sum(D()[2:4,2])/sum(D()[2:4,1])
               }
               
               BHcond2 = ifelse(BH3, paste('Using a cutscore of B3 the test is clinically efficient.'),
                                paste0('The test is not clinically efficient using a cutscore of B3. ',
                                       'For the test to be generalized clinically efficient, the cost of false negatives ',
                                       'to false positives would need to be at least ', round(kLB3, 2),  
                                       ' and no more than ', round(kUB3, 2), '.'))
               
               return(BHcond2)
               
          })
          
          output$ppvPlot1 <- renderPlot({
               
               # create dataset
               k = c(seq(.01, .99, length = 20), seq(1, 3, length = 20), seq(3.1, 20, length = 100))
               P = data.frame(k = c(k, k), Prob = c(1/(1+k), 1/(1+(1/k))), 
                              type = rep(c('PPV', 'NPV'), each = length(k)))
               ksig = ifelse(D()[1,1] > D()[1,2], 1, round(sum(D()[1,2])/sum(D()[1,1]),2))
               d3 = data.frame(k = c(ksig,ksig), Prob = c(sum(D()[1,1])/sum(D()[1,1:2]), sum(D()[2:4,2])/sum(D()[2:4,1:2])), 
                               type = c('PPV', 'NPV'))
               
               ppvPlot1 = 
                    ggplot(data = P, aes(y = Prob, x = k, col = type)) + geom_line(size = 1.2) + 
                    geom_ribbon(data = P[P$type == 'PPV',], aes(x = k, ymin = Prob, ymax = 1), 
                                col = 'blue', fill = 'blue', alpha = .5) +
                    geom_ribbon(data = P[P$type == 'NPV',], aes(x = k, ymin = Prob, ymax = 1), 
                                col = 'red', fill = 'red', alpha = .5) +
                    scale_color_manual(name = '', values = c('red','blue'), 
                                       labels = c('NPV','PPV')) + 
                    theme(legend.position = 'bottom') +
                    scale_x_continuous('k', limits = c(0, max(k)), breaks = c(ksig,5,10,15,20),
                                       labels = c(ksig,'5','10','15','20')) +   
                    scale_y_continuous('Probability', limits = c(0,1)) +
                    geom_vline(x = ksig, lty = 2) + 
                    geom_hline(y = D()[1,1]/sum(D()[1,1:2]), lty = 2) + 
                    geom_hline(y = sum(D()[2:4,2])/sum(D()[2:4,1:2]), lty = 2) + 
                    geom_text(data = d3, aes(label = type), hjust = ifelse(ksig < 5, -5, -.5), 
                              vjust = -.5, col = 'black') 
                    
               print(ppvPlot1)
          })
          
          output$ppvPlot2 <- renderPlot({
               
               # create dataset
               k = c(seq(.01, .99, length = 20), seq(1, 3, length = 20), seq(3.1, 20, length = 100))
               P = data.frame(k = c(k, k), Prob = c(1/(1+k), 1/(1+(1/k))), 
                              type = rep(c('PPV', 'NPV'), each = length(k)))
               ksig = ifelse(sum(D()[1:2,1]) > sum(D()[1:2,2]), 1, round(sum(D()[1:2,2])/sum(D()[1:2,1]),2))
               d3 = data.frame(k = c(ksig,ksig), Prob = c(sum(D()[1:2,1])/sum(D()[1:2,1:2]), sum(D()[3:4,2])/sum(D()[3:4,1:2])), 
                               type = c('PPV', 'NPV'))
               
               ppvPlot2 = 
                    ggplot(data = P, aes(y = Prob, x = k, col = type)) + geom_line(size = 1.2) + 
                    geom_ribbon(data = P[P$type == 'PPV',], aes(x = k, ymin = Prob, ymax = 1), 
                                col = 'blue', fill = 'blue', alpha = .5) +
                    geom_ribbon(data = P[P$type == 'NPV',], aes(x = k, ymin = Prob, ymax = 1), 
                                col = 'red', fill = 'red', alpha = .5) +
                    scale_color_manual(name = '', values = c('red','blue'), 
                                       labels = c('NPV','PPV')) + 
                    theme(legend.position = 'bottom') +
                    scale_x_continuous('k', limits = c(0, max(k)), breaks = c(ksig,5,10,15,20),
                                       labels = c(ksig,'5','10','15','20')) +   
                    scale_y_continuous('Probability', limits = c(0,1)) +
                    geom_vline(x = ksig, lty = 2) + 
                    geom_hline(y = sum(D()[1:2,1])/sum(D()[1:2,1:2]), lty = 2) +
                    geom_hline(y = sum(D()[3:4,2])/sum(D()[3:4,1:2]), lty = 2) + 
                    geom_text(data = d3, aes(label = type), hjust = ifelse(ksig < 5, -5, -.5), 
                              vjust = -.5, col = 'black') 
                              
               print(ppvPlot2)
          })
          
     })

