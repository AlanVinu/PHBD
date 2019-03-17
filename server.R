library(shiny)

function(input, output) {
  a = 0
  output$patient <- renderTable({
    OriginalRecords <-
      read.csv("~/R/MainProjectPhase1/Personalised_Healthcare/OriginalRecords.csv")
    OriginalRecords[, 1] <- NULL
    rownames(OriginalRecords) <- c(1:nrow(OriginalRecords))
    #To find a
    a = which(OriginalRecords[,"PatientId"] == input$PatientID)
    
    Q <- as.matrix(OriginalRecords[a, ])
    return(Q)
  })
  output$riskPlot <- renderPlot({
    OriginalRecords <-
      read.csv("~/R/MainProjectPhase1/Personalised_Healthcare/OriginalRecords.csv")
    OriginalRecords[, 1] <- NULL
    #To find a
    a = which(OriginalRecords[,"PatientId"] == input$PatientID)
    
    #To find Ja
    Ja <- c()
    j = 1
    for (i in 6:10) {
      if (OriginalRecords[a, i] == TRUE) {
        Ja[j] = i
        j = j + 1
      }
    }
    
    #To find nJa
    nJa <- c()
    j = 1
    for (i in 6:10) {
      if (OriginalRecords[a, i] == FALSE) {
        nJa[j] = i
        j = j + 1
      }
    }
    
    #To find AV
    AV <- c()
    for (i in 1:length(nJa)) {
      AV[i] <-
        (as.numeric(table(OriginalRecords[nJa[i]][OriginalRecords[nJa[i]] == TRUE]))) /
        nrow(OriginalRecords)
    }
    
    #To find I
    I <- c(6:10)
    
    #To find K
    sumofIFVSofallpatients <-
      foreach(i = 1:nrow(OriginalRecords), .combine = "+") %dopar% {
        #for(i in 1:nrow(OriginalRecords)) {
        sumofIFVS = 0
        
        #To find Ji
        Ji <- c()
        j = 1
        for (k in 6:10) {
          if (OriginalRecords[i, k] == TRUE) {
            Ji[j] = k
            j = j + 1
          }
        }
        
        for (j in 1:length(I)) {
          #Inverse Frequency f
          f = log(nrow(OriginalRecords) / (as.numeric(table(
            OriginalRecords[I[j]][OriginalRecords[I[j]] == TRUE]
          ))))
          
          #To find num1
          num1 = f * OriginalRecords[a, I[j]] * f * OriginalRecords[i, I[j]]
          deno1 = 0
          for (k in 1:length(Ja)) {
            #Inverse Frequency f
            f = log(nrow(OriginalRecords) / (as.numeric(table(
              OriginalRecords[Ja[k]][OriginalRecords[Ja[k]] == TRUE]
            ))))
            deno1 = deno1 + (f ^ 2) * (OriginalRecords[a, Ja[k]] ^ 2)
          }
          deno1 = sqrt(deno1)
          deno2 = 0
          for (k in 1:length(Ji)) {
            #Inverse Frequency f
            f = log(nrow(OriginalRecords) / (as.numeric(table(
              OriginalRecords[Ji[k]][OriginalRecords[Ji[k]] == TRUE]
            ))))
            deno2 = deno2 + (f ^ 2) * (OriginalRecords[i, Ji[k]] ^ 2)
          }
          deno2 = sqrt(deno2)
          sumofIFVS = sumofIFVS + (num1 / (deno1 * deno2))
        }
        #sumofIFVSofallpatients = sumofIFVSofallpatients + sumofIFVS
        sumofIFVS
      }
    K = 1 / sumofIFVSofallpatients
    #start_time <- Sys.time()
    prediction <- c()
    for (b in 1:length(nJa)) {
      predictionscore <- c()
      for (c in 1:length(Ja)) {
        sumofIFVSofallpatients = 0
        PatientRecords <-
          OriginalRecords[OriginalRecords[[Ja[c]]] == TRUE,]
        AVC <-
          (as.numeric(table(PatientRecords[nJa[b]][PatientRecords[nJa[b]] == TRUE]))) /
          nrow(PatientRecords)
        if (AVC > AV[b]) {
          sumofIFVSofallpatients <-
            foreach(i = 1:nrow(PatientRecords),
                    .combine = "+") %dopar% {
                      #for(i in 1:nrow(PatientRecords)) {
                      sumofIFVS = 0
                      
                      #To find Ji
                      Ji <- c()
                      j = 1
                      for (k in 6:10) {
                        if (PatientRecords[i, k] == TRUE) {
                          Ji[j] = k
                          j = j + 1
                        }
                      }
                      
                      for (j in 1:length(I)) {
                        #Inverse Frequency f
                        f = log(nrow(PatientRecords) / (as.numeric(table(
                          PatientRecords[I[j]][PatientRecords[I[j]] == TRUE]
                        ))))
                        
                        #To find num1
                        num1 = f * OriginalRecords[a, I[j]] * f * PatientRecords[i, I[j]]
                        if (num1 != 0) {
                          deno1 = 0
                          for (k in 1:length(Ja)) {
                            #Inverse Frequency f
                            f = log(nrow(PatientRecords) / (as.numeric(
                              table(PatientRecords[Ja[k]][PatientRecords[Ja[k]] == TRUE])
                            )))
                            deno1 = deno1 + (f ^ 2) * (OriginalRecords[a, Ja[k]] ^ 2)
                          }
                          deno1 = sqrt(deno1)
                          deno2 = 0
                          for (k in 1:length(Ji)) {
                            #Inverse Frequency f
                            f = log(nrow(PatientRecords) / (as.numeric(
                              table(PatientRecords[Ji[k]][PatientRecords[Ji[k]] == TRUE])
                            )))
                            deno2 = deno2 + (f ^ 2) * (PatientRecords[i, Ji[k]] ^ 2)
                          }
                          deno2 = sqrt(deno2)
                          sumofIFVS = sumofIFVS + (num1 / (deno1 * deno2))
                        }
                      }
                      #sumofIFVSofallpatients = sumofIFVSofallpatients + sumofIFVS
                      sumofIFVS
                    }
          predictionscore[c] = AVC + K * (1 - AVC) * sumofIFVSofallpatients
        } else{
          sumofIFVSofallpatients <-
            foreach(i = 1:nrow(PatientRecords),
                    .combine = "+") %dopar% {
                      #for(i in 1:nrow(PatientRecords)) {
                      sumofIFVS = 0
                      
                      #To find Ji
                      Ji <- c()
                      j = 1
                      for (k in 6:10) {
                        if (PatientRecords[i, k] == TRUE) {
                          Ji[j] = k
                          j = j + 1
                        }
                      }
                      
                      for (j in 1:length(I)) {
                        #Inverse Frequency f
                        f = log(nrow(PatientRecords) / (as.numeric(table(
                          PatientRecords[I[j]][PatientRecords[I[j]] == TRUE]
                        ))))
                        
                        #To find num1
                        num1 = f * OriginalRecords[a, I[j]] * f * PatientRecords[i, I[j]]
                        if (num1 != 0) {
                          deno1 = 0
                          for (k in 1:length(Ja)) {
                            #Inverse Frequency f
                            f = log(nrow(PatientRecords) / (as.numeric(
                              table(PatientRecords[Ja[k]][PatientRecords[Ja[k]] == TRUE])
                            )))
                            deno1 = deno1 + (f ^ 2) * (OriginalRecords[a, Ja[k]] ^ 2)
                          }
                          deno1 = sqrt(deno1)
                          deno2 = 0
                          for (k in 1:length(Ji)) {
                            #Inverse Frequency f
                            f = log(nrow(PatientRecords) / (as.numeric(
                              table(PatientRecords[Ji[k]][PatientRecords[Ji[k]] == TRUE])
                            )))
                            deno2 = deno2 + (f ^ 2) * (PatientRecords[i, Ji[k]] ^ 2)
                          }
                          deno2 = sqrt(deno2)
                          sumofIFVS = sumofIFVS + (num1 / (deno1 * deno2))
                        }
                      }
                      #sumofIFVSofallpatients = sumofIFVSofallpatients + sumofIFVS
                      sumofIFVS
                    }
          predictionscore[c] = AV[b] + K * (1 - AV[b]) * sumofIFVSofallpatients
        }
      }
      prediction[b] <- max(predictionscore)
    }
    
    A <- c()
    names <- c()
    k = 1
    for (i in 1:length(nJa)) {
      if (prediction[i] != 0 && nJa[i] != 9 && nJa[i] != 10) {
        A[k] <- prediction[i]
        names[k] <- nJa[i]
        k = k + 1
      }
    }
    for (i in 1:length(names)) {
      prediction <- A
      nJa <- names
    }
    
    FinalPrediction <-
      matrix(
        prediction * 100,
        nrow = 1,
        ncol = length(nJa),
        dimnames = list(c(1), c(colnames(
          OriginalRecords
        )[nJa]))
      )
    
    # Render a barplot
    text(
      barplot({
        FinalPrediction
      },
      main = input$PatientID,
      ylim = c(0, 100),
      ylab = "Risk Percentage",
      xlab = "Disease"),
      FinalPrediction + 5,
      paste(round(FinalPrediction, digits = 2), "%", sep = "")
    )
  })
}