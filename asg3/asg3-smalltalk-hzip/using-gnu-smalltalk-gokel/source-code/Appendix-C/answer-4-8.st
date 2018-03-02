"answer_4_8.st"

| newNumber howManyNumbers sum arithmeticAverage |

newNumber := 0.

howManyNumbers := 0.
sum := 0.
arithmeticAverage := 0.

[newNumber = 'finish'] whileFalse: [
	Transcript show: 'Arithmetic averages of the numbers so far is: ', arithmeticAverage printString; cr.
	
	Transcript show: 'Please enter a new number or "finish" to exit: '.
    newNumber := stdin nextLine.
    
    (newNumber isNumeric) ifTrue: [
	    sum := sum + newNumber asInteger.
	    howManyNumbers := howManyNumbers + 1.
	    arithmeticAverage := sum / howManyNumbers.
	].
	
	Transcript cr.
].
