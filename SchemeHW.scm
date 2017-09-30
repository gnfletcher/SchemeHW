;SchemeHW Authors: Greg Fletcher, 

;Main function that takes list of data from user, sequential passes list to add counter list, check patient data and increment appropriate atom in counter list and calculate conditional probability
;and returns a four integer and one string list containing probablity calculations and qualitative decision based on data.
(define (medical_test Patient_Data)
	(prob_list(count(new_list('((0 0) (0 0) (0 0) (0 0)) 'Patient_Data))))
)

;Adds list to car of Patient Data passed by user to be used to keep track of test results.
;This list while be passed to the probability calculator function.
;List format: ((PostiveTest1(if deseased) PositiveTest1(total)) (NegativeTest1(if healthy) NegativeTest1(total)) (PostiveTest2(if deseased) PositiveTest2(total)) (NegativeTest2(if healthy) NegativeTest2(total)))
(define (new_list Counts Patient_Data)
	(cons 'Counts 'Patient_Data)
)

;Counts takes list of data from main function (medical_test), checks if only the 8 atom list is left in the list, passes list to calc_prob function if true else passes the 8 atom list and the next patient data to evaluate_patient then pops the first two lists and adds the new 8 atom list back.	
(define (count Patient_Data)
		(cond	((null? cdr 'Patient_Data) 'Patient_Data)
				(else counts(cons evaluate_patient((car 'Patient_Data) (cadr 'Patient_Data)) (cddr 'Patient_Data))))
)	

;Takes counts list and next patient data from count and determines if patient is sick, if so passes to sick patient else to healthy patient.
(define (evaluate_patient Counts Patient)
	(list (car (test_1 (car Counts) (cadr Counts) 'Patient) car (test_2 (caddr Counts) (cadddr Counts) 'Patient)))
)

;Test_1_Result
(define (test_1 Pos_1 Neg_1 Patient)
	(cond ((= (caddr 'Patient) 1) 
		(cond ((= (cadr 'Patient) 1) (list (incr_both 'Pos_1) 'Neg_1)))
		(else (list (incr_total 'Pos_1) 'Neg_1))))
	(else (cond ((= (cadr 'Patient) 0) (list 'Pos_1 (incr_both 'Neg_1))))
		(else (list 'Pos_1 (incr_total 'Neg_1))))
)

;Test_2_Result
(define (test_2 Pos_Sick Pos_Total Patient)
	(cond ())
)

;Increments both atoms of the list.
(define (incr_both List)
	(list (+ 1 (car 'List)) (+ 1 (cadr 'List)))
)

;Increments first atom in the list.
(define (incr_a List)
	(list (+ 1 (car 'List)) (cadr 'List))
)

;Increments the second atom in the list.
(define (incr_total List)
	(list (car 'List) (+ 1 (cadr 'List)))
)

;Calculates the conditonal probability of the passed list. Calculated as first atom / second atom.
(define (calc_prob List)
	(/ (car 'List) (cadr 'List)
)

;Takes the list of counts lists and individually passes them to the probability calculator and returns a list of probabilities.
(define (prob_list Counts)
	(list (calc_prob (car Counts)) (calc_prob (cadr Counts)) (calc_prob (caddr Counts)) (calc_prob (cadddr Counts)))
)
