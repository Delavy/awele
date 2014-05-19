addData.getDecalage <-
function (listOfFx) 
{
    decal = 0
    for (i in 1:length(listOfFx)) {
        decal = decal + listOfFx[[i]]@decal
    }
    return(decal)
}
