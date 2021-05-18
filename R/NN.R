#lambda function used in the first NN
lambda_function=function(param) {
  out=param[,1]+param[,2]*param[,3]
}




#Building and fitting the NN for mu0 and tau
first_NN = function(X_train,Y_train,T_train,x,y,t,X_val,Y_val,T_val,arcitecture=c(128,64,32),epochs_t=30){

  `%>%` <- magrittr::`%>%`

  #Defining the inputs of the NN

  in1_firstNN=keras::layer_input(shape=c(ncol(X_train)),name="X_input")
  in2_firstNN=keras::layer_input(shape=c(ncol(T_train)),name="t_input")

  #Define network architecture

  pred=in1_firstNN%>%keras::layer_dense(units=arcitecture[1],activation='relu')
  for (j in c(1:(length(arcitecture)-1))){
    pred=pred%>%keras::layer_dense(units=arcitecture[j+1],activation = 'relu')
  }
  pred=pred%>%keras::layer_dense(units=2,activation='linear',name='test1')



  pred2=keras::layer_concatenate(c(pred,in2_firstNN),name="test2")%>%
    keras::layer_lambda(lambda_function)

  #Compiling model
  model=keras::keras_model(inputs=c(in1_firstNN,in2_firstNN),outputs=pred2)


  model%>%keras::compile(
    optimizer = 'adam',
    loss = 'mean_squared_error',
    metrics = c('mean_absolute_error')
  )

  #defining earlystopping
  earlystopping_t = keras::callback_early_stopping(patience=10,restore_best_weights = TRUE,mode="min")


  #fit model


  model %>% keras::fit(
    x = list(X_train,T_train),
    y = Y_train,
    epochs = epochs_t,
    validation_data=list(list(X_val,T_val),Y_val),
    callbacks=earlystopping_t
  )

  #get output for mu0 and tau

  layer_name='test1'

  inter_output_model = keras::keras_model(inputs = model$input,outputs = keras::get_layer(model, layer_name)$output)
  inter_output = predict(inter_output_model,list(x,t))

  mu0output=as.matrix(inter_output[,1])
  tauoutput=as.matrix(inter_output[,2])


  #get output for y

  output1=predict(model,list(x,t))

  y_output=as.matrix(output1)

  funcout=list(mu0output,tauoutput,y_output)
  return(funcout)
}



#Building and fitting the NN for prospensity scores

second_NN=function(X_t,T_t,x,t,x_v,t_v,arcitect=c(128,64,32),epochs_p=30){

  `%>%` <- magrittr::`%>%`

  #Defining inputs

  in_2NN=keras::layer_input(shape=c(ncol(X_t)))

  #Defining architecture of second NN
  pred_2NN=in_2NN%>%keras::layer_dense(units=arcitect[1],activation = 'relu')

  for (z in c(1:(length(arcitect)-1))){
    pred_2NN=pred_2NN%>%keras::layer_dense(units=arcitect[z+1],activation = 'relu')
  }

  pred_2NN=pred_2NN%>%keras::layer_dense(units=1,activation = 'sigmoid')

  #Compile model
  modelps=keras::keras_model(inputs=in_2NN,outputs=pred_2NN)



  modelps%>%keras::compile(
    optimizer = 'adam',
    loss = 'binary_crossentropy',
    metrics = c('accuracy')
  )

  #defining earlystopping
  earlystopping_p = keras::callback_early_stopping(patience=10,restore_best_weights = TRUE,mode="min")

  #fit model

  modelps%>%keras::fit(
    x = X_t,
    y = T_t,
    epochs = epochs_p,
    validation_data=list(x_v,t_v),
    callbacks=earlystopping_p
  )

  out2=predict(modelps,x)

}
