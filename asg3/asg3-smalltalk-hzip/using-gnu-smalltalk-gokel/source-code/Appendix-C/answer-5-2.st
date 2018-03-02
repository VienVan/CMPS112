"answer_5_2.st"

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

Human subclass: Man [
    | money handsomeness |
    
    setMoney: amountOfMoney [
        "Amount of money out of 10"
        
        money := amountOfMoney.
    ]
    
    getMoney [
        ^money
    ]
    
    setHandsomeness: rateOfHandsomeness [
        "Handsomeness rate out of 10"
        
        handsomeness := rateOfHandsomeness.
    ]
    
    getHandsomeness [
        ^handsomeness
    ]
    
    > aMan [
        (self getName = 'Canol Gökel') ifTrue: [
            ^true
        ] ifFalse: [
            ^(self getMoney + self getHandsomeness) > (aMan getMoney + aMan getHandsomeness)
        ]
    ]
    
    < aMan [
        (self getName = 'Canol Gökel') ifTrue: [
            ^false
        ] ifFalse: [
            ^(self getMoney + self getHandsomeness) < (aMan getMoney + aMan getHandsomeness)
        ]
    ]
    
    = aMan [
        (self getName = 'Canol Gökel') ifTrue: [
            ^false
        ] ifFalse: [
            ^(self getMoney + self getHandsomeness) = (aMan getMoney + aMan getHandsomeness)
        ]
    ]
]

Human subclass: Woman [
    | honesty generosity |
    
    setHonesty: rateOfHonesty [
        "Honesty rate out of 10"
        
        honesty := rateOfHonesty.
    ]
    
    getHonesty [
        ^honesty
    ]
    
    setGenerosity: rateOfGenerosity [
        "Generosity rate out of 10"
        
        generosity := rateOfGenerosity.
    ]
    
    getGenerosity [
        ^generosity
    ]
    
    > aWoman [
        ^(self getHonesty + self getGenerosity) > (aWoman getHonesty + aWoman getGenerosity)
    ]
    
    < aWoman [
        ^(self getHonesty + self getGenerosity) < (aWoman getHonesty + aWoman getGenerosity)
    ]
    
    = aWoman [
        ^(self getHonesty + self getGenerosity) = (aWoman getHonesty + aWoman getGenerosity)
    ]
]

| man1 man2 man3 woman1 woman2 |

man1 := Man new.
man1 setName: 'Michael Cooper'.
man1 setAge: 32.
man1 setMoney: 9.
man1 setHandsomeness: 7.

man2 := Man new.
man2 setName: 'Paul Anderson'.
man2 setAge: 28.
man2 setMoney: 7.
man2 setHandsomeness: 8.

man3 := Man new.
man3 setName: 'Canol Gökel'.
man3 setAge: 24.
man3 setMoney: 1.
man3 setHandsomeness: 1.

woman1 := Woman new.
woman1 setName: 'Louise Stephney'.
woman1 setAge: 26.
woman1 setHonesty: 6.
woman1 setGenerosity: 7.

woman2 := Woman new.
woman2 setName: 'Maria Brooks'.
woman2 setAge: 32.
woman2 setHonesty: 8.
woman2 setGenerosity: 6.

(man1 > man2) printNl.
(man1 < man2) printNl.
(man3 > man1) printNl.
(woman1 > woman2) printNl.
(woman1 < woman2) printNl.
