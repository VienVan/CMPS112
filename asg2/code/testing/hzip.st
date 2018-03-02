#!/usr/local/bin/gst -f



"------- Mckeys Code --------"
nl := Character nl.
Character extend [
   isPrint [
	  ^ (Character space < self) & (self <= $~)
   ]

   visible [
	  self isPrint ifTrue: [^ self asString]
				   ifFalse:
				   [| x |
						x := self asInteger.
						x = 10 ifTrue: [^ 'x0A']
						ifFalse: [x = 32 ifTrue: [^ 'x20']
						ifFalse: [x = 256 ifTrue: [^ 'EOF']
						ifFalse: [^ 'DNE']]]
					]
   ]

]

FileStream extend [
   |bitIndex currentChar|

   nextBit [
	  |bit|
	  bitIndex isNil ifTrue: [bitIndex := 0].
	  bitIndex = 0 ifTrue: [
		 bitIndex := 8.
		 currentChar := self next.
	  ].
	  bit := currentChar value bitAt: bitIndex.
	  bitIndex := bitIndex - 1.
	  ^ bit
   ]

   atBitEnd [
	  ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
   ]

]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
	  self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
	  |result|
	  result := super new.
	  result init: size.
	  ^ result
   ]

   init: size [
	  theArray := Array new: size.
   ]

   size [
	  ^ theArray size.
   ]

   at: index [
	  ^ theArray at: index + 1.
   ]

   at: index put: value [
	  ^ theArray at: index + 1 put: value.
   ]

   incr: index [
	  (self at: index) isNil
		 ifTrue: [ self at: index put: 0].
	  self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
	  (0 to: (self size) - 1) do: [:index |
		 aBlock value: index value: (self at: index).
	  ]
   ]

]


Object subclass: Leaf [
 nl := Character nl.
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
	  self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
	  |result|
	  result := super new.
	  result setChar: aChar andCount: aCount.
	  ^result
   ]

   setChar: aChar andCount: aCount [
	  char := aChar.
	  count := aCount.
   ]

   <= other [
	  ^ (count < other count)
	  | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
	  ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
	  (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
	  visitor value: char value: string.
   ]

   postorder: visitor prefix: string [
	  visitor value: char value: string.
   ]

   isLeaf [^true]

]


Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
	  self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
	  |result|
	  result := super new: aChar count: aCount.
	  result setLeft: aLeft andRight: aRight.
	  ^ result
   ]

   setLeft: aLeft andRight: aRight [
	  left := aLeft.
	  right := aRight.
   ]

   getLeft [
	  left = nil ifTrue: [^ 'DNE'] ifFalse: [^left].
   ]

   getRight [
	  right = nil ifTrue: [^ 'DNE'] ifFalse: [^right].
   ]

   printOn: aStream [
	  (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
	  left inorder: visitor prefix: string, '0'.
	  right inorder: visitor prefix: string, '1'.
   ]

   postorder: visitor prefix: ostream[
	  left postorder: visitor prefix: ostream.
	  right postorder: visitor prefix: ostream.
	  ostream << 1.

   ]

   isLeaf [^false]

]


Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
	  self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
	  |result|
	  result := super new.
	  result init: fileStream.
	  ^result
   ]
   clearByte [
	  bitIndex := 8.
	  currentByte := 0.
   ]
   init: fileStream [
	  myStream := fileStream.
	  self clearByte.
   ]
   flushByte [
	  bitIndex = 8 ifFalse: [^ currentByte asCharacter
	  ].
   ]
   writeBit: bit [
	  currentByte := currentByte bitAt: bitIndex put: bit.
	  bitIndex := bitIndex - 1.

   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
	  |result|
	  result := super new.
	  result init: fileStream.
	  ^result
   ]
   init: fileStream [
	  myStream := fileStream.
	  bitIndex := 9.
	  byte := myStream.
   ]
   nextBit [
	  bitIndex := bitIndex - 1.
	  ^byte value bitAt: bitIndex
   ]
   atEnd [
	  ^bitIndex = 1.
   ]
   currByte [
	  ^byte
   ]
]
"--------- Mckeys Code-End ---------------"

Object subclass: Hzip
[

	| inStream outStream operand freqtable priorityQueue encodingTree |
	| tempHolderFreq tempHolderArray |
	nl := Character nl.

	Hzip class >> new
	[
	   self shouldNotImplement
	]

	Hzip class >> new: option infile: stream outfile: ostream
	[   | result |
		result := super new.
		result init: option infile: stream outfile: ostream.
		result checkOption.
		"result compressAndWrite."
		"result printEncodingTree."

		^result.
	]

	encode [
		self initFreqTable.
		self insertInPriorityQueue.
		self buildEncodingTree.
		self insertEncodingTreeInFreq.
		self insertFreqInArray.
	]

	checkOption
	[
		operand = '-t' ifTrue: [self optionT.].
		operand = '-c' ifTrue: [self optionC.].
		operand = '-u' ifTrue: [self optionU.].
		operand = '-d' ifTrue: [self optionD.].
	]

	init: option infile: stream
	[
		operand := option.
		inStream := FileStream open: stream mode: FileStream read.
	]

	init: option infile: stream outfile: ostream
	[
		operand := option.

		inStream := FileStream open: stream mode: FileStream read.
		(ostream isKindOf: FileStream)
			ifFalse:
			[
				outStream := FileStream open: ostream mode: FileStream write.
			]
			ifTrue: [outStream := ostream.].

	]

	initFreqTable
	[
		| temp |
		temp := inStream.
		freqtable := ZeroArray new: 257.
		[temp atEnd] whileFalse:
		[
			temp nextLine do: [ :x | freqtable incr: x asInteger].
			freqtable incr: 10.
		].
		freqtable at: 256 put: 1.
	]

	insertInPriorityQueue
	[
		| char leaf|
		priorityQueue := SortedCollection new.

		freqtable keysAndValuesDo: [:key :value |
			char := key asCharacter.
			(value notNil and: [value > 0]) ifTrue:
			[
				leaf := Leaf new: char count: value.
				priorityQueue add: leaf.
			].
		].
	]

	buildEncodingTree
	[
		[priorityQueue size > 1] whileTrue: [
		   |first second tree|
		   first := priorityQueue removeFirst.
		   second := priorityQueue removeFirst.
		   tree := Tree new: (first char)
			   count: first count + second count
			   left: first
			   right: second.
		   priorityQueue add: tree.
		   ].
	]

	insertEncodingTreeInFreq
	[
		tempHolderFreq := ZeroArray new: 257.
		encodingTree := priorityQueue removeFirst.

		encodingTree inorder:
		[ :char :binary |
			tempHolderFreq at: char asInteger put: binary.
		] prefix: ''.
	]

	insertFreqInArray
	[
		| i |
		tempHolderArray := Array new: 257	.
		i := 1.
		tempHolderFreq keysAndValuesDo:
		[:key :value|
			(value notNil) ifTrue:
			[
				tempHolderArray at:i put:value.
				i := i + 1.
			].
		].
	]

	output
	[
		| i str val|
		i := 1.
		freqtable keysAndValuesDo:
		[:key :value|
			(value notNil) ifTrue:
			[
				str := key asCharacter visible.
				str size to: 1 do: [ :skip| outStream << ' '].
				outStream << str.
				val := value asString size.
				(str size = 1)
				ifTrue: [val to: 9 do: [:skip| outStream << ' ']]
				ifFalse: [val to: 8 do: [:skip| outStream << ' ']].

				outStream<< value << '  '.
				outStream<< (tempHolderArray at: i) << nl.
				i := i + 1.
			].
		].
	]

	"TODO: insert EOF"
	compressAndWrite
	[ | y |

		inStream reset.
		[inStream atEnd] whileFalse:
		[
			inStream nextBit do:
			[ :x |
				y := tempHolderFreq at: (x asInteger).
				outStream nextPutAll: y.
			].
			outStream nextPutAll: (tempHolderFreq at: 10).

		].
	]

	"Preq: Encoding tree is empty"
	"48: 0, 49: 1"
	"removeFirst: first element entered"
	"removeLast: last element entered"
	decompress
	[
		| flag i stack count store bitArray outBytes|
		| readNext8Bits checkEof val rightChild leftChild tree|

		checkEof := false.
		flag := true.
		readNext8Bits := false.
		stack := OrderedCollection new.
		count := 0.

		[inStream atEnd] whileFalse:
		[
			inStream nextLine do:
			[ :x |
				i := x asInteger.
				flag ifTrue:
				[
					(readNext8Bits not)
					ifTrue:
					[
						(i = 48)
						ifTrue:
						[
							outBytes := OutBits new: stdout.
							readNext8Bits := true.
						]
						ifFalse:
						[
							checkEof ifTrue:
							[
								stack add:
									(Leaf new:(256 asCharacter) count: 0).
									checkEof := false
							]
							ifFalse:
							[
								rightChild := stack removeLast.
								stack size = 0 ifTrue:
								[
									encodingTree := rightChild.
									store := encodingTree.
									flag := false.
								]
								ifFalse:
								[
									leftChild := stack removeLast.
									tree := Tree new: $s
									count: 0
									left: leftChild
									right: rightChild.
									stack add: tree.
								]
							].
						].
					]
					ifFalse:
					[
						count := count + 1.
						(i = 48)
						ifTrue: [outBytes writeBit: 0 asInteger]
						ifFalse: [outBytes writeBit: 1 asInteger].

						(count = 8) ifTrue:
						[
							readNext8Bits := false.
							count := 0.
							val := outBytes flushByte.
							(val asInteger = 0) ifTrue: [checkEof := true]
							ifFalse: [stack add: (Leaf new: val count: 0)].
						].
					].
				]
				ifFalse:
				[

					(x asInteger = 48)
					ifTrue: [store := store getLeft]
					ifFalse: [store := store getRight].

					(store isLeaf)
					ifTrue:[outStream << store char. store := encodingTree].
				].
			].
		].
	]

	optionT
	[
		self encode.
		self output.
	]


	optionC
	[
		| stdinBits |
		self encode.
		encodingTree postorder: [ :char :binary|
			outStream << (0 bitAt: 1) asInteger.
			stdinBits := BitStream new: char.
			[stdinBits atEnd] whileFalse: [
				outStream << stdinBits nextBit.
			].
			(char asInteger = 256) ifTrue: [outStream << 1].

		] prefix: outStream.
		outStream << 1.
		self compressAndWrite.
	]

	optionU
	[
		self decompress.
	]

	optionD
	[
		self decompress.
	]

]

argArray := Smalltalk arguments.
sizeArgv := argArray size.

freqtable := ZeroArray new: 257.
priorityQueue := SortedCollection new.

| inFile outFile|

((sizeArgv >= 1) & (sizeArgv < 4)) ifTrue:
[
	| x y z|
	operand := argArray at: 1.
	inFile := argArray at: 2.
	(sizeArgv = 3)
		ifTrue: [outFile := argArray at: 3]
		ifFalse: [outFile := stdout.].

	hzip := Hzip new: operand infile: inFile outfile: outFile.
]
ifFalse:
[
	stdout << 'Usage: hzip.st [-t|-c|-u|-d] <infile> <outfile>' << nl
].
