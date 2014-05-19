dpln.adlsupp.create.model <-
function (dataset, listOfFx) 
{
    dataset = addData.completeData(dataset, listOfFx)
    decal = addData.getDecalage(listOfFx)
    selection = dataset[dataset[, 14 + decal] == "G", ]
    model = LDA(selection[, 1:(12 + decal)], selection[, 13 + 
        decal])
    return(model)
}
