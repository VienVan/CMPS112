"answer_3_7.st"

| definitions theWord |

definitions := Dictionary new.

definitions at: 'programming language' put: 'We call the command groups which provide a communication between us and computer hardware as programming languages.'.
definitions at: 'source code' put: 'The code we wrote in a programming language (not the result of a compilation or interpretation process) is called source code.'.
definitions at: 'virtual machine' put: 'The converting process from byte-code to machine code is done by programs called virtual machines.'.
definitions at: 'cross-platform' put: 'Cross-platform is the name for being able to run a software on different computer architectures, like different operating systems or different processors.'.

Transcript show: 'Please enter a word to get its definition: '.
theWord := stdin nextLine.

Transcript show: theWord, ': ', (definitions at: theWord asLowercase); cr.
