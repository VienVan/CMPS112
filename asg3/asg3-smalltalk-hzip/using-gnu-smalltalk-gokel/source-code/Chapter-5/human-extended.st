"human_extended.st"
"A program which has an extended version of Human class."

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
		Transcript show: 'Hello, my name is ', name, ' and I''m ', age printString, ' years old.'; cr.
	]
	
	> aHuman [
		^age > aHuman getAge
	]
	
	< aHuman [
		^age < aHuman getAge
	]
	
	= aHuman [
		^age = aHuman getAge
	]
]

Human extend [
	| occupation experience |

	getOccupation [
		^occupation
	]

	setOccupation: anOccupation [
		occupation := anOccupation.
	]

	getExperience [
		^experience
	]

	setExperience: anExperience [
		experience := anExperience.
	]

	introduceYourself [
		Transcript show: 'Hello, my name is ', name, ' and I''m ', age printString, ' years old. I am ', occupation, '.'; cr.
	]

	> aHuman [
		^experience > aHuman getExperience
	]

	< aHuman [
		^experience < aHuman getExperience
	]

	= aHuman [
		^experience = aHuman getExperience
	]
]

| me myFriend |

me := Human new.
me setName: 'Canol Gökel'.
me setAge: 24.
me setOccupation: 'an Engineer'.
me setExperience: 1.

myFriend := Human new.
myFriend setName: 'İsmail Arslan'.
myFriend setAge: 23.
myFriend setOccupation: 'an Engineer'.
myFriend setExperience: 3.

me introduceYourself.
myFriend introduceYourself.

(me > myFriend) printNl.
