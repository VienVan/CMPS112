"answer_4_4.st"

| theLength |

Transcript show: 'Please enter the length of the edge of the diamond: '.
theLength := stdin nextLine asInteger.

1 to: theLength do: [ :x |
    1 to: (theLength - x) do: [ :y |
        Transcript show: ' '.
    ].
    
    Transcript show: '/'.
    
    1 to: ((x - 1) * 2) do: [ :y |
        Transcript show: ' '.
    ].
    
    Transcript show: '\'; cr.
].

1 to: theLength do: [ :x |
    1 to: (x - 1) do: [ :y |
        Transcript show: ' '.
    ].
    
    Transcript show: '\'.
    
    1 to: ((theLength - x) * 2) do: [ :y |
        Transcript show: ' '.
    ].
    
    Transcript show: '/'; cr.
].
