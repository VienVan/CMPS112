#!/usr/local/bin/gst -f
"#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f"
nl := Character nl.
"
 3 PARTNER 1: VIEN VAN vhvan
 4 PARTNER 2: AKOBIR KHAMIDOV akhamido"
"vien is awesome"
"------------------------------- Mckeys Code ---------------------------------"

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
						ifFalse: [^ x printStringRadix: 8]]]
					]
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
	  ^ (count < other count) | ((count = other count) & (char <= other char))
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

   postorder: visitor[
	  visitor value: char value: false.
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

   postorder: visitor[
	  left postorder: visitor.
	  right postorder: visitor.

	  visitor value: char value: true.

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
	  bitIndex = 8 ifFalse: [
		 myStream nextPutByte: currentByte.
		 self clearByte.
	  ]
   ]
   writeBit: bit [
	  currentByte := currentByte bitAt: bitIndex put: bit.
	  bitIndex := bitIndex - 1.
	  bitIndex = 0 ifTrue: [self flushByte].
   ]
   getByte [
   ^ currentByte.
   ]
]

Object subclass: OutBitsMutated [
   |bitIndex currentByte myStream|
   OutBitsMutated class >> new [
	  self shouldNotImplement.
   ]
   OutBitsMutated class >> new: fileStream [
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
   getByte [
	^currentByte.
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
	  bitIndex := 1.
   ]
   nextBit [
	  bitIndex = 1 ifTrue: [
		 byte := myStream next.
		 bitIndex := 9.
	  ].
	  bitIndex := bitIndex - 1.
	  ^byte value bitAt: bitIndex
   ]
   atEnd [
	  ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
	  ^byte
   ]
   getBitIndex [
   ^ bitIndex.
   ]

   setToEnd [myStream setToEnd]
]

Object subclass: BitStreamMutated [
   |bitIndex byte myStream|
   BitStreamMutated class >> new: fileStream [
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
"--------------------- Mckeys Code-End --------------------------"

Object subclass: Hzip
[
	| inStream outStream operand freqtable priorityQueue encodingTree |
	| tempHolderFreq tempHolderArray outbits|
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
			"freqtable incr: 10."
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
		tempHolderArray := Array new: 257.
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
	[ | y stdinBits bit|

		inStream reset.

		[inStream atEnd] whileFalse:
		[

			inStream nextLine do:
			[ :x |
				y := tempHolderFreq at: x asInteger.
				y do:
				[ :x |
					outbits writeBit: ((x asInteger) - 48).
				].
			].
			y := tempHolderFreq at: 10.
			"EOF"
			y do:
			[ :x |
				outbits writeBit: ((x asInteger) - 48).
			].
		].

		y := tempHolderFreq at: 256.
		"EOF"
		y do:
		[ :x |
			outbits writeBit: ((x asInteger) - 48).
		].
		outbits flushByte.
	]

	"Preq: Encoding tree is empty"
	"48: 0, 49: 1"
	"removeFirst: first element entered"
	"removeLast: last element entered"

	decompress
	[
		|x flag bitstream i var stack store  byteString checkEof val rightChild leftChild tree |

		x := 0.
		checkEof := false.
		flag := true.

		stack := OrderedCollection new.

		bitstream := BitStream new: inStream.

		[bitstream atEnd] whileFalse:
		[
			i := bitstream nextBit.
			flag ifTrue:[
				i = 0 ifTrue: [
					byteString := OutBitsMutated new: stdout.
					1 to: 8 do:
					[ :x |
						byteString writeBit: bitstream nextBit.
					].
					var := byteString flushByte.
					var asInteger = 0 ifTrue: [
						checkEof := true.
					]
					ifFalse: [
						stack add: (Leaf new: byteString flushByte count: 0).
					]

				]

				ifFalse:
				[
					checkEof ifTrue:
					[
						stack add: (Leaf new:(256 asCharacter) count: 0).
						checkEof := false.
					]
					ifFalse:
					[
						leftChild := stack removeLast.
						(stack isEmpty) ifTrue:
						[
							encodingTree := leftChild.
							store := encodingTree.
							flag := false.
						]
						ifFalse:
						[
							rightChild := stack removeLast.
							tree := Tree new: leftChild
							count: 0
							left: rightChild
							right: leftChild.
							stack add: tree.
						]

					]

				]
			]
			ifFalse:
			[
				store isLeaf ifTrue:
				[
					bitstream setToEnd.
				] ifFalse:
				[
					x asInteger = 256 ifFalse:
					[
						(i asInteger = 0)
						ifTrue: [store := store getLeft]
						ifFalse: [store := store getRight].

						(store isLeaf)
						ifTrue:
						[
							x := store char.
							x asInteger = 256 ifFalse:
							[
								outStream nextPutByte: x asInteger.
								store := encodingTree.
							] ifTrue:
							[
								bitstream setToEnd.
							]
						].
					].
				].

			].
		]

	]



	optionT
	[
		self encode.
		self output.
	]


	optionC
	[
		| stdinBits x |
		self encode.
		outbits := OutBits new: outStream.
		"outbits := OutBits new: outStream."
		encodingTree postorder: [ :char :isReturn|
			isReturn ifTrue: [outbits writeBit: 1]
			ifFalse: [
			outbits writeBit: 0.
			stdinBits := BitStreamMutated new: char.
			[stdinBits atEnd] whileFalse: [
				x := (stdinBits nextBit).
				outbits writeBit: x.
			].
			(char asInteger = 256) ifTrue: [outbits writeBit: 1].
		 ].

		].
		outbits writeBit: 1.
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
ifFalse: [stdout << 'Usage: hzip.st [-t|-c|-u|-d] <infile> <outfile>' << nl].
