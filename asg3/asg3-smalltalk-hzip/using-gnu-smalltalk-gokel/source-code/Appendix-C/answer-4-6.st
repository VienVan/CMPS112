"answer_4_6.st"

99 to: 1 by: -1 do: [ :x |
    Transcript show: x printString, ' bottles of beer on the wall, ', x printString, ' bottles of beer.'; cr.
    
    (x > 1) ifTrue: [
        Transcript show: 'Take one down and pass it around, ', (x - 1) printString, ' bottles of beer on the wall.'; cr.
    ] ifFalse: [
        Transcript show: 'Take one down and pass it around, no more bottles of beer on the wall.'; cr.
    ].
    
    Transcript cr.
].

Transcript show: 'No more bottles of beer on the wall, no more bottles of beer.'; cr.
Transcript show: 'Go to the store and buy some more, 99 bottles of beer on the wall.'; cr.
