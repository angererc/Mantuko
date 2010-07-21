"Extensions for making writing classes with accessors a bit less verbose"

ClassDescription extend [

    accessors: aString [
        self getters: aString.
        self setters: aString.
    ]

    getters: aString [
        aString substrings do: [:ivar |
            (self allInstVarNames includes: ivar asSymbol)
                ifFalse: [ self addInstVarName: ivar ].
            self createGetMethod: ivar.
        ]
    ]

    setters: aString [
        aString substrings do: [:ivar |
            (self allInstVarNames includes: ivar asSymbol)
                ifFalse: [ self addInstVarName: ivar ].
            self createSetMethod: ivar.
        ]
    ]

]