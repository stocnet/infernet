# a confusion matrix prints, also under its old class

    Code
      print(out)
    Output
      # Confusion matrix of a logistic MRQAP of ties
      # From 50 draws of the predicted probabilities, with 95% intervals
      
            Predicted
      Actual 0             1            
           0 29.3 [25, 34] 18.3 [13, 25]
           1 18.7 [14, 23] 23.7 [17, 29]
      
       accuracy sensitivity specificity precision   f1  auc threshold
          0.589       0.564        0.61     0.558 0.56 0.75       0.5
       threshold.accuracy nobs draws
                    0.711   90    50

