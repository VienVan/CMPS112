"answer_3_6.st"

| theNumbers arithmeticAverage |

Transcript show: 'Please enter two numbers separated by space to get their arithmetic average: '.
theNumbers := stdin nextLine tokenize: ' '.

arithmeticAverage := ((theNumbers at: 1) asInteger + (theNumbers at: 2) asInteger) / 2.

Transcript show: 'The arithmetic average of ', (theNumbers at: 1), ' and ',  (theNumbers at: 2), ' is ', arithmeticAverage printString, '.'; cr.
