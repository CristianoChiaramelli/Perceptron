#Written by Cristiano Di Maio Chiaramelli

#function that creates the model (Initializing random weigts)
generate.model <- function(input.size){

	# weigths = [theta; w_1; w_2; ...; w_k]
	weigths = cbind(runif(input.size+1, -1, 1))

	return (weigths)
}

#function that trains the model, given the dataset
train <- function(model, eta=0.01, threshold=0.01, train.dataset){


	#forward.datset is the train.dataset without the desired outputs
	forward.dataset = train.dataset[,1:ncol(train.dataset)-1]
	desired = train.dataset[,ncol(train.dataset)]

	total_sqr_error=threshold
	while(total_sqr_error >= threshold){
		total_sqr_error=0

		for (input in 1:nrow(forward.dataset)){

			#Calculate the output with the current weigths
			output = forward(model=model, input=forward.dataset[input,])

			#Calculate the error of this input
			error = desired[input] - output
			
			#Change weigths
			model = model + eta*error*append(1,forward.dataset[input,])

			total_sqr_error=total_sqr_error+error*error
		}

		cat("Squared error: ", total_sqr_error, "\n")
	}

	return (model)
}

#function that runs a test dataset
test <- function(model, test.dataset){

	output = as.numeric(vector(length=nrow(test.dataset)))

	for (input in 1:nrow(test.dataset)){	
		output[input] = forward(model, test.dataset[input,])
	}
	#Returns all the results
	return (output)
}

#Applies the sigmoid function
sigmoid <- function(x){
	return (1/(1+exp(-x)))
}

#Applies a given input with the current weigths
forward <- function(model, input){

	output = input%*%model[2:nrow(model)]+model[1]

	return (sigmoid(output))
}