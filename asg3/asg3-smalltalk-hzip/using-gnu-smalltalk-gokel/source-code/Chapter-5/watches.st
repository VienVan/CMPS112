"watches.st"
"A program which defines classes about watches to demonstrate the usage of the super keyword."

Object subclass: Watch [
    | style chronometerCapability |
    
    <comment: 'A class for defining ordinary watches.'>
    
    setStyle: theStyle [
        "A method to set the style of the watch."
	    
        style := theStyle.
    ]
    
    getStyle [
        "A method to get the style of the watch."
	    
        ^style
    ]
    
    setChronometerCapability: theChronometerCapability [
        "A method to specify if the watch has chronometer capabilities."
	    
        chronometerCapability := theChronometerCapability.
    ]
    
    getChronometerCapability [
        "A method to determine if the watch has chronometer capabilities."
	    
        ^chronometerCapability.
    ]
    
    listYourFeatures [
        "A method to print out the features of the watch."
	    
        Transcript show: 'Style: ', self getStyle; cr.
        Transcript show: 'Chronometer capabilities: ', self getChronometerCapability printString; cr.
    ]
]

Watch subclass: WaterResistantWatch [
    | resistanceDepth |
    
    <comment: 'A class for defining water resistant watches.'>
    
    setResistanceDepth: aDepth [
        "A method to set the resistance depth of the watch."
	    
        resistanceDepth := aDepth.
    ]
    
    getResistanceDepth [
        "A method to get the resistance depth of the watch."
	    
        ^resistanceDepth.
    ]
    
    listYourFeatures [
        "A method to print out the features of the water resistant watch."
	    
        self listYourFeatures.
	    
        Transcript show: 'Resistance depth: ', self getResistanceDepth printString; cr.
    ]
]

| watch1 watch2 |

watch1 := Watch new.
watch1 setStyle: 'Analog'.
watch1 setChronometerCapability: true.

watch2 := WaterResistantWatch new.
watch2 setStyle: 'Digital'.
watch2 setChronometerCapability: false.
watch2 setResistanceDepth: 30.

Transcript show: 'The features of the 1st watch:'; cr.
Transcript show: '------------------------------'; cr.
watch1 listYourFeatures.

Transcript cr.

Transcript show: 'The features of the 2nd watch:'; cr.
Transcript show: '------------------------------'; cr.
watch2 listYourFeatures.
