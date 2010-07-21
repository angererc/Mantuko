CharacterArray extend [
	asCapitalized [
		self size == 0 ifTrue: [
			^ self.
		] ifFalse: [
			^ self copyReplaceFrom:1 to: 1 withObject: (self first asUppercase).
		]
	]
]

Dictionary extend [
	valueForKey: aKey [
		^ self at:aKey asSymbol.
	]
	
	setValue: aValue forKey: aKey [
		self at:aKey asSymbol put:aValue.
	]
]

Object extend [
	Object class >> accessInstanceVariablesDirectly [
		<category: 'key-value coding'>
		^ true.
	]
	
	Object class >> getterForKey: aKey [
		"later, this computation could be cached in some dictinary"
		| selector |
		
		selector := aKey asSymbol.
		(self canUnderstand:selector) ifTrue: [
			^ selector.
		] ifFalse: [
			selector := ('get', aKey asCapitalized) asSymbol.
			(self canUnderstand:selector) ifTrue: [
				^ selector.
			] ifFalse: [
				^ nil.
			]
		]
	]
	
	Object class >> setterForKey: aKey [
		"I answer a selector for a setter for the given key, if i find one. Otherwise, nil.
		(I try either setTheKey: or just theKey:)
		later, this computation could be cached in some dictinary"
		| capitalizedKey selector|
		capitalizedKey := (aKey asCapitalized, ':') asSymbol.
		
		selector := ('set', capitalizedKey) asSymbol.
		(self canUnderstand:selector) ifTrue: [
			^ selector
		] ifFalse: [
			selector := capitalizedKey asSymbol.
			(self canUnderstand:selector) ifTrue: [
				^ selector
			] ifFalse: [
				^ nil.
			]
		]
	]
	
	Object class >> ivarForKey: aKey [
		"later, this computation could be cached in some dictinary"
		| ivars ivar |
				
		ivars := self allInstVarNames.
		ivar := aKey asSymbol.
		
		(ivars includes:ivar) ifTrue: [
			^ ivar
		] ifFalse: [
			ivar := ('is', aKey asCapitalized) asSymbol.
			(ivars includes:ivar) ifTrue: [
				^ ivar
			] ifFalse: [
				^ nil.
			]
		]
	]
	
	valueForKey:aKey [
		"I answer the member of self with name aSymbol. I try to invoke a getter method if one exists.
		If not, I check whether self class permits accessInstanceVariablesDirectly.
		If so, I try to return the instance variable with the name aSymbol.
		If no such variable exitst, I call valueForUndefinedKey:aSymbol on self.
		"
		| class getter ivar |

		class := self class.
		getter := class getterForKey:aKey.
		
		getter ifNotNil: [
			^ self perform:getter.
		] ifNil: [
			class accessInstanceVariablesDirectly ifTrue: [
				ivar := class ivarForKey:aKey.
				ivar ifNotNil: [
					^ self instVarNamed:ivar.
				]
			]
		].
		
		^ self valueForUndefinedKey:aKey.
	]
	
	valueForKeyPath: aPathString [
		| components value |
		components := aPathString subStrings.
		value := self.
		
		components do: [ :component |
			value := value valueForKey:component.
		].
		
		^ value.
	]
	
	setValue: aValue forKey: aKey [
		| class modifier ivar |

		class := self class.		
		modifier := class setterForKey:aKey.		
		modifier ifNotNil: [
			^ self perform:modifier with:aValue.
		] ifNil: [
			(class accessInstanceVariablesDirectly) ifTrue: [				
				ivar := class ivarForKey:aKey.
				Transcript show:('trying direct access to: ', ivar); cr.
				ivar ifNotNil: [
					self instVarNamed:ivar put:aValue.
					^ self.
				]
			]
		].
		self setValue:aValue forUndefinedKey:aKey.
	]
	
	setValue: aValue forKeyPath: aPathString [
		| components owner |
		
		(aPathString isNil or:[aPathString isEmpty]) ifTrue: [
			^ nil.
		].
				
		components := aPathString subStrings.
		owner := self.
		
		components allButLast do: [ :component |
			owner := owner valueForKey:component.
		].
		
		^ owner setValue:aValue forKey:components last.
	]
	
	valueForUndefinedKey: aKey [
		SystemExceptions.NotFound 
				signalOn:self 
					what:(self class printString, ' is not key-value compliant for key: ', aKey printString, '. Getter ').
	]
	
	setValue: aValue forUndefinedKey: aKey [
		SystemExceptions.NotFound 
				signalOn:self 
					what:(self class printString, ' is not key-value compliant for key: ', aKey printString, '. Setter ').
	]
]