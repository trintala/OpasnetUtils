IntArray <-
function(input1, input2, rescol2name = "input2result") {
if (is.array(input1)) input1 <- as.data.frame(as.table(input1))
MatchingIndices <- names(dimnames(input2))[names(dimnames(input2))%in%colnames(input1)]
if (length(MatchingIndices)==0) warning("No matching indices", call. = FALSE)
ArrayExtraIndices <- names(dimnames(input2))[names(dimnames(input2))%in%colnames(input1)==FALSE]
if (length(MatchingIndices[MatchingIndices=="obs"])>0) input1[,"obs"] <- as.factor(as.character(input1[,"obs"]))
DataframeMatchingLocs <- NA
if (length(MatchingIndices)>0) {for (i in 1:length(MatchingIndices)) {
DataframeMatchingLocs <- levels(factor(input1[,MatchingIndices[i]]))[levels(factor(input1[,MatchingIndices[i]]))
%in%dimnames(input2)[[MatchingIndices[i]]]]
if (length(DataframeMatchingLocs)==0) stop(paste("No matching locations in the index:", MatchingIndices[i]))
input1 <- input1[input1[,MatchingIndices[i]]%in%DataframeMatchingLocs,]
}}
if (length(ArrayExtraIndices)>0) {for (i in 1:length(ArrayExtraIndices)) {
input1 <- input1[rep(1:nrow(input1), each = length(dimnames(input2)[[ArrayExtraIndices[i]]])),]
input1[,ncol(input1) + 1] <- dimnames(input2)[[ArrayExtraIndices[i]]]
colnames(input1)[ncol(input1)] <- ArrayExtraIndices[i]
}}
input1[,ncol(input1) + 1] <- input2[as.matrix(input1[,names(dimnames(input2))])]
colnames(input1)[ncol(input1)] <- rescol2name
return(input1[is.na(input1[,rescol2name])==FALSE,])
}

