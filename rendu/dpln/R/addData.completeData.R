addData.completeData <-
function (data, listOfFx) 
{
    decal = addData.getDecalage(listOfFx)
    newData = data.frame(matrix(data = 0, nr = nrow(data), nc = ncol(data) + 
        decal))
    asColNames = length(colnames(data)) > 0
    for (i in 1:12) {
        newData[, i] = data[, i]
        if (asColNames) {
            colnames(newData)[i] = colnames(data)[i]
        }
    }
    pos = 13
    for (i in 1:length(listOfFx)) {
        newData = listOfFx[[i]]@fonction(newData, pos)
        pos = pos + listOfFx[[i]]@decal
    }
    if (ncol(newData) > (12 + decal)) {
        for (i in (13 + decal):ncol(newData)) {
            newData[, i] = data[, i - decal]
            colnames(newData)[i] = colnames(data)[i - decal]
        }
    }
    return(newData)
}
