#Residuals plot
windows()
par(mfrow=c(1,2))
plot(model,1,add.smooth=FALSE)
plot(model,2)

windows()
par(mfrow=c(1,2))
plot(spreadModel,1,add.smooth=FALSE)
plot(spreadModel,2)