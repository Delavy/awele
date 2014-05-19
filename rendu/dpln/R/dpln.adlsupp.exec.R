dpln.adlsupp.exec <-
function (awele, model, listOfFx) 
{
    g = graines.matrix(awele)
    g = addData.completeData(g, listOfFx)
    colnames(g)[1:12] = c(paste("J", 1:6, sep = ""), paste("A", 
        1:6, sep = ""))
    prediction = predict(model, data.frame(g), type = "raw")
    ret = data.frame(matrix(data = 0, ncol = 6, nrow = 1))
    colnames(ret) = levels(prediction)
    ret[c(prediction[1])] = 1
    return(ret)
}
