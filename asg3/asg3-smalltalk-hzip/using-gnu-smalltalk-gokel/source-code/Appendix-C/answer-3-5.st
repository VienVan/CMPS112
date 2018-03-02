"answer_3_5.st"

| theNumber |

Transcript show: 'Please enter a number to get its cube: '.
theNumber := stdin nextLine.

Transcript show: 'The cube of ', theNumber, ' is ', (theNumber asInteger squared * theNumber asInteger) printString, '.'; cr.
