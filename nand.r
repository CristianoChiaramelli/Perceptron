
#Function set and run the NAND problem
run_nand <- function(eta=0.01, threshold=0.01){
	#Generates de NAND values
	#x1	x2	output
	#0 	0 	1
	#0 	1 	1
	#1 	0 	1
	#1 	1 	0

	train.dataset = t(matrix( c(0,0,1,
								0,1,1,
								1,0,1,
								1,1,0), nrow=3))


	test.dataset = train.dataset[,1:2]

	model = generate.model(ncol(test.dataset))

	model = train(model=model, train.dataset = train.dataset, eta=eta, threshold=threshold)

	output = test(model=model, test.dataset = test.dataset)

	cat("\nResulting weigths:\nbias\tweight1\tweight2\n")
	cat(model, "\n\n")

	cat ("Result of test set:\n")
	cbind (test.dataset, output)
}