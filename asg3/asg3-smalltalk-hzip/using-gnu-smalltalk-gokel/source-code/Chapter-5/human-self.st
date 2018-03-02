"human_self.st"
"The second version of the Human class written using self keywords."

Object subclass: Human [
	| name age |

	setName: aName [
		name := aName.
	]

	getName [
		^name
	]

	setAge: anAge [
		age := anAge.
	]

	getAge [
		^age
	]

	introduceYourself [
		Transcript show: 'Hello, my name is ', self getName, ' and I''m ', self getAge printString, ' years old.'; cr.
	]

	> aHuman [
		^self getAge > aHuman getAge
	]

	< aHuman [
		^self getAge < aHuman getAge
	]

	= aHuman [
		^self getAge = aHuman getAge
	]
]

| me myBrother | 

me := Human new. 
me setName: 'Canol Gökel'. 
me setAge: 24. 

myBrother := Human new. 
myBrother setName: 'Gürol Gökel'. 
myBrother setAge: 27. 

me introduceYourself. 
myBrother introduceYourself. 

(me < myBrother) printNl.
