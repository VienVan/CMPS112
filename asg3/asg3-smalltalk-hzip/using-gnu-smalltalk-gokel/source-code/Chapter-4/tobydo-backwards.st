"tobydo_backwards.st"
"A program to demonstrate the backward capability of to:by:do: message."

5 to: 1 by: -1 do: [:x |
	Transcript show: 'Oh my god! I''m counting backwards! This is the ', x printString, '. line!'; cr.
]
