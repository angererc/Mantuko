FileStream fileIn:'kvc.st'.

Object subclass: Face [
	| target |
	<comment: 'I represent a face for an object.'>
	
	"
	lenses: a lens must be a mapping from symbols (the face eyes) to components.
	Components can be 
	* key paths or symbols 
	* (#foo transformReads:aReadBlock writes:aWriteBlock)
	* Face responsibility and Face responsibility:theInitialValue for face internal variables
	
	If a lens is underspecified, an identity mapping is used
	
	attaching a lens to an object will also result in the generation of accessors on the object
	that is, attaching MyFace with name #foo to object obj will generate a 'obj fooFace' method.

	"
	
	"todos:
	allow for fast-path when attaching
	lazy attaching (can be solved by adding a method such as positionFace[ FooFace attachTo:self ]?)
	generate fooHasChanged method for an eye foo"
	instanceID := 0.
	generatedLensClasses := Dictionary new.
	generatedObjectProxyClasses := Dictionary new.
	
	Face class >> attachTo: anObject withName: aSymbol [
		" use the identity lens "
		^ self attachTo: anObject withName: aSymbol lens: {}.
	]
	
	Face class >> attachTo: anObject withName: aSymbol lens: aLens [
		" if a lens is underspecified, we use identity mapping"
		| lensObjAssoc subclass face |

		"cache the generated classes as a function of self->lens tuples
		because the code is defined by the face class and applied lens"
		lensObjAssoc := self->aLens.
		(generatedLensClasses includesKey:lensObjAssoc) ifTrue: [
			subclass := generatedLensClasses at:lensObjAssoc.
		] ifFalse: [
			subclass := (self generateSubclassForLens:aLens).
			generatedLensClasses at:lensObjAssoc put:subclass.
		].
		
		"some components in the lense might want to instantiate fields that they generated in the class"
		face := subclass new initWithTarget:anObject.
		self instantiateFields:face withLens: aLens.
		self generateDefaultFaceAccessor:aSymbol forFace:face onObject:anObject.
		^ face.
	]
	
	Face class >> generateDefaultFaceAccessor:aSymbol forFace:aFace onObject:anObject [
		| class instVarMap method |
		
		"first check that we don't have this accessor already"
		(anObject respondsTo:('%1Face'%{aSymbol asString}) asSymbol) ifTrue: [
			Exception new signal:'object %1 does already contain a face with name: ''%2''.'%{anObject printString. aSymbol asString}.
		].
		
		class := anObject class.
		
		(anObject respondsTo:#isFacesProxy) ifFalse: [
			(generatedObjectProxyClasses includes:class) ifTrue:[
				class := generatedObjectProxyClasses at:class.
			] ifFalse: [
				"generate a proxy class, inject a field into the object, and change the object class"
				method := class kindOfSubclass 
					    , 'instanceVariableNames:classVariableNames:poolDictionaries:category:'.

				class := class perform: method asSymbol
				    withArguments: 
					{('FacesProxy_', class name) asSymbol.
					'faces_attachedFaces'.
					''.
					''.
					'Faces -- generated'}.
			
				class primCompile:'isFacesProxy [ ^ true. ]'.
			].
			
			"class is a proper subclass, so we can simply map from old position to old position in the object shape"
			instVarMap := Array new:class allInstVarNames size.
			1 to: anObject class allInstVarNames size do: [ :i |
				instVarMap at:i put:i.
			].
			"change the object shape"
			class mutate:anObject via:instVarMap.
			"change the class of the object. It is important that this is done after the mutation because otherwise
			the old values are not copied correctly"
			anObject changeClassTo: class.
			
			"instantiate the dictionary"
			anObject instVarNamed:#faces_attachedFaces put:Dictionary new.
			
		].
		
		(anObject instVarNamed:#faces_attachedFaces) at:aSymbol put:aFace.
		class primCompile:'%1Face [ ^ faces_attachedFaces at:#%1 ]'%{aSymbol asString}.
	]
	
	Face class >> instantiateFields:aFace withLens:aLens [
		| eye eyeTarget |
		aLens do: [ :assoc |
			eye := assoc key.
			eyeTarget := assoc value.
			(eyeTarget respondsTo:#instantiateFields:forEye:) ifTrue: [
				eyeTarget instantiateFields:aFace forEye:eye.
			].
		].
	]
	
	Face class >> generateSubclassForLens: aLens [
		| definedEyes observedEyes subclass eye eyeTarget |
		instanceID := instanceID + 1.
		subclass := self 
						subclass:('Face_Nr', instanceID printString) asSymbol
						instanceVariableNames:''
						classVariableNames:''
						poolDictionaries:''
						category:'Faces -- generated'.
			
		"sanity check"
		definedEyes := self faces_declaredEyes subStrings asSet.
		(definedEyes size >= aLens size) ifFalse:[ 
			Exception new signal:'size of lens %1 does not match size of defined eyes: ''%2''.'%{aLens printString. self faces_declaredEyes}.
		].

		(self respondsTo:#faces_observedEyes) ifTrue: [
			observedEyes := self faces_observedEyes subStrings asSet.
		] ifFalse: [
			observedEyes := {}.
		].
		observedEyes do: [ :observed |			
			subclass addInstVarName:('face_%1Observed'%{observed}) asSymbol.
			subclass primCompile:'%1DidChange [ ^ face_%1Observed ~= self %1. ]'%{observed}.
		].
		
		aLens do: [ :assoc |
			eye := assoc key.
			eyeTarget := assoc value.
			
			(definedEyes includes:(eye asString)) ifFalse: [
				Exception new signal:'Illegal eye %1 of lens is not in defined eyes: %2.'%{eye printString. self faces_declaredEyes}.
			].
			definedEyes remove:eye asString.
			
			"fail fast if wrong values in lens"
			(eyeTarget respondsTo:#compileEye:into:cacheReads:) ifFalse: [
				Exception new signal:'Illegal lens component: %1. Does not respond to selector #compileEye:into:cacheReads:'%{eyeTarget printString}.
			].
			eyeTarget compileEye:eye into:subclass cacheReads:(observedEyes includes:eye asString).
		].
		
		definedEyes do: [ :eye |
			"eye are guaranteed to be strings here, because they came from one"
			eye compileEye:eye asSymbol into:subclass cacheReads:(observedEyes includes:eye).
		].
						
		^ subclass.
	]
	
	Face class >> responsibility [
		" answer a lens component that results in a field of the face instead of a binding to the target object"
		^ self responsibility:nil
	]
	
	Face class >> responsibility:anInitialValue [
		^ FaceInternalField new initWithInitialValue:anInitialValue.
	]
	
	initWithTarget: anTarget [
		target := anTarget.
	]
	
	target [
		^ target.
	]
	
]

Object subclass: FaceInternalField [
	"can be put in a lens; generates an instance variable plus setter and getter in the lens
	can be obtained through Face responsibility and Face responsibility:"
	| initialValue |
	
	initWithInitialValue: anInitialValue [
		initialValue := anInitialValue.
	]
	
	compileEye:anEye into: aClass cacheReads: aObserveFlag [
		| eyeString code field |
		eyeString := anEye asString.
		
		field := 'faces_%1Internal'%{eyeString}.
		aClass addInstVarName: field asSymbol.
		aObserveFlag ifTrue:[
			code := '%1 [ face_%1Observed := %2. ^ face_%1Observed. ]' % {eyeString. field.}.
		] ifFalse: [
			code := '%1 [ ^ %2]' % {eyeString. field.}.
		].
		aClass primCompile:code.
		
		code := '%1: aValue [ %2 := aValue ]' % {eyeString. field.}.
		aClass primCompile:code.
		
	]
	
	instantiateFields: aFace forEye:anEye [
		aFace instVarNamed:'faces_%1Internal'%{anEye asString} put:initialValue.
	]
]

Object subclass: ValueTransformer [
	"a value transformer can be put in a lens. It contains two blocks and
	generates a reader and writer for an eye (plus some instance variables) that goes through those blocks
	a value transformer can be obtained through the transformReads:writes: method on String"
	| keyPath readTransformer writeTransformer |
	initWithKeyPath:aKeyPath readTransformer:aRT writeTransformer:aWT [
		keyPath := aKeyPath.
		readTransformer := aRT.
		writeTransformer := aWT.
	]
	
	keyPath [ ^ keyPath ]
	readTransformer [ ^ readTransformer ]
	writeTransformer [ ^ writeTransformer ]
	
	compileEye:anEye into: aClass cacheReads: aObserveFlag [
		| eyeString code transformerInstanceField |
		eyeString := anEye asString.
		
		transformerInstanceField := 'faces_%1TransformRead'%{eyeString}.
		aClass addInstVarName: transformerInstanceField asSymbol.
		aObserveFlag ifTrue: [
			code := '%1 [ face_%1Observed := %2 value:(target valueForKeyPath:''%3''). ^ face_%1Observed. ]' % {eyeString. transformerInstanceField. keyPath asString}.
		] ifFalse: [
			code := '%1 [ ^ %2 value:(target valueForKeyPath:''%3'')]' % {eyeString. transformerInstanceField. keyPath asString}.
		].
		aClass primCompile:code.
		
		transformerInstanceField := 'faces_%1TransformWrite'%{eyeString}.
		aClass addInstVarName: transformerInstanceField asSymbol.
		code := '%1: aValue [ target setValue: (%2 value:aValue) forKeyPath:''%3'']' % {eyeString. transformerInstanceField. keyPath asString}.
		aClass primCompile:code.
	]
	
	instantiateFields: aFace forEye:anEye [
		aFace instVarNamed:'faces_%1TransformRead'%{anEye asString} put:readTransformer.
		aFace instVarNamed:'faces_%1TransformWrite'%{anEye asString} put:writeTransformer.
	]
]

"add annotations to the compiler"
ClassDescription extend [

    eyes: aString [
		| code |
		code := 'faces_declaredEyes [^''%1'']' % {aString asString}.
        self class primCompile:code.
	]
	
	observeEyes: aString [
		| code |
		code := 'faces_observedEyes [^''%1'']' % {aString asString}.
        self class primCompile:code.
	]

]

"add transformation helpers to string"
String extend [
	transformReads:aReadBlock writes:aWriteBlock [
		^ ValueTransformer new initWithKeyPath:self readTransformer:aReadBlock writeTransformer:aWriteBlock.
	]
	
	compileEye:anEye into: aClass cacheReads: aObserveFlag [
		| code |
		aObserveFlag ifTrue: [
			code := '%1 [ face_%1Observed := target valueForKeyPath:''%2''. ^ face_%1Observed. ]' % {anEye asString. self asString}.
		] ifFalse: [
			code := '%1 [ ^ target valueForKeyPath:''%2'']' % {anEye asString. self asString}.
		].		
		aClass primCompile:code.
		code := '%1: aValue [ target setValue: aValue forKeyPath:''%2'']' % {anEye asString. self asString}.
		aClass primCompile:code.
	]
]