"answer_4_5.st"

| theNumber isPrime i |

Transcript show: 'Please enter a number: '.
theNumber := stdin nextLine asInteger.

i := 2.

[theNumber \\ i = 0] whileFalse: [
	i := i + 1.
].

(i = theNumber) ifTrue: [
	Transcript show: theNumber printString, ' is a prime number.'; cr.
] ifFalse: [
	Transcript show: theNumber printString, ' is not a prime number. It is devidable by ', i printString, '.'; cr.
].
