PackageLoader fileInPackage:'SUnit'.
FileStream fileIn:'faces.st'.

"run with gst-sunit -f faces_test.st FacesTest"

Face subclass: MyFace [
	<eyes:'one two three'>
	<observeEyes:'one two three'>
	decOne [
		self one:self one - 1.
	]
]

TestCase subclass: FacesBasicTests [
	| objABC1 objABC2 lens1A2B3C lens3A1B2C |
	
	setUp [
		objABC1 := Dictionary from: {#a -> 42. #b -> 43. #c -> 44.}.
		objABC2 := Dictionary from: {#a -> 99. #b -> 100. #c -> 101.}.
		
		lens1A2B3C := {#one->#a. #two->#b. #three->#c}.
		lens3A1B2C := {#three->'a'. #one->'b'. #two->'c'}.
	]
	
	testFacesCachesGeneratedClass [
		| face1 face2 |
		"same face and equal lens should result in same class"
		face1 := MyFace attachTo:objABC1 withName:#face lens:lens1A2B3C.
		face2 := MyFace attachTo:objABC2 withName:#face lens:lens1A2B3C copy.
		self assert:(face1 class == face2 class).
	]
	
	testFacesGeneratesClassesForDifferentLenses [
		| face1 face2 |
		face1 := MyFace attachTo:objABC1 withName:#face lens:lens1A2B3C.
		face2 := MyFace attachTo:objABC2 withName:#face lens:lens3A1B2C.
		self assert:(face1 class ~= face2 class).
	]
	
	testReadWrite [
		| face1 |
		face1 := MyFace attachTo:objABC1 withName:#face lens:lens1A2B3C.
		self assert:(face1 one == 42).
		self assert:(face1 two == 43).
		self assert:(face1 three == 44).
		face1 decOne.
		self assert:(face1 one == 41).
		self assert:((objABC1 at:#a) == 41).
	]
]

TestCase subclass: FacesValueTransformerTests [
	| obj lens |
	
	setUp [
		obj := Dictionary from: {#a -> 42. #three->#notyet}.
		
		lens := {#one->(#a transformReads:[:v| v asString] writes:[:v| v asInteger]).
				#two->#b.}.
	]
	
	testCreateValueTransformers [
		| vt |
		"from symvol"
		vt := #a transformReads:[] writes:[].
		self assert:(vt class == ValueTransformer).
		
		"from string"
		vt := 'a b c' transformReads:[] writes:[].
		self assert:(vt class == ValueTransformer).
	]
	
	testUseValueTransformer [
		| face |
		face := MyFace attachTo:obj withName:#face lens:lens.
		self assert:(face one = '42').
		
		face one:'99'.
		self assert:((obj at:#a) = 99).
	]
	
	testUnderspecifiedLensResultsInIdentityMapping [
		| face |
		face := MyFace attachTo:obj withName:#face.
		face three:'i changed'.
		self assert:(face three = 'i changed').
		self assert:(obj at:#three) = 'i changed'.
	]
		
]

TestCase subclass: FacesInternalFieldsTests [
	| obj lens |
	
	setUp [
		obj := Dictionary from: {#a -> 42}.
		
		lens := {#one->(#a transformReads:[:v| v asString] writes:[:v| v asInteger]).
				#two->Face responsibility.
				#three->(Face responsibility:'initial value').}.
	]
	
	testFaceInternalFields [
		| face |
		face := MyFace attachTo:obj withName:#face lens:lens.
		self assert:(face two = nil).
		face two:666.
		self assert:(face two = 666).
		self assert:(face three = 'initial value').
	]
		
]

TestCase subclass: FacesAccessorMethodsTests [
	| obj lens |
	
	setUp [
		obj := Dictionary from: {#a -> 42}.
		
		lens := {#one->(#a transformReads:[:v| v asString] writes:[:v| v asInteger]).
				#two->Face responsibility.
				#three->(Face responsibility:'initial value').}.
	]
	
	testFaceDefaultAccessor [
		| face1 face2 proxyClass |
		self deny:(obj respondsTo:#isFacesProxy) description:'1'.
		
		face1 := MyFace attachTo:obj withName:#someName lens:lens.
		self assert:(obj respondsTo:#isFacesProxy) description:'2'.
		self assert:(obj isFacesProxy) description:'3'.
		self assert:(obj respondsTo:#someNameFace) description:'2b'.
		
		proxyClass := obj class.
		face2 := MyFace attachTo:obj withName:#someOtherName.
		self assert:(obj class == proxyClass)  description:'4'. "didn't change the proxy class"
		
		self assert: obj someNameFace == face1  description:'5'.
		self assert: obj someOtherNameFace == face2  description:'6'.
		
		obj := Dictionary new. "attach to new object and check that the generated class was cached"
		face2 := MyFace attachTo:obj withName:#someName.
		self assert:(obj class == proxyClass)  description:'4b'. "didn't change the proxy class"
		
		obj := Set new.
		face2 := MyFace attachTo:obj withName:#someName.
		self assert:(obj class ~= proxyClass)  description:'4c'. "But for different objects it should change"
	]
		
]

TestCase subclass: FacesObserveOldValuesTests [
	| obj lens |
	
	setUp [
		obj := Dictionary from: {#a -> 42. #b->43.}.
		
		lens := {#one->#a.
				#two->(#b transformReads:[:v| v asString] writes:[:v| v asInteger]).
				#three->Face responsibility.}.
	]
	
	testObserving [
		| face |
		face := MyFace attachTo:obj withName:#foo lens:lens.
		
		self assert:(face oneDidChange) description:'1'.
		self assert:(face twoDidChange) description:'2'.
		self deny:(face threeDidChange) description: '3: 3 is a face internal field that was initialized to nil'.
		
		"DidChange implies a read, therefore it's all false now"
		self deny:(face oneDidChange) description:'1b'.
		self deny:(face twoDidChange) description:'2b'.
		self deny:(face threeDidChange) description: '3b'.
		
		face one:99.
		face two:'1234'.
		face three:200.
		self assert:(face oneDidChange) description:'1c'.
		self assert:(face twoDidChange) description:'2c'.
		self assert:(face threeDidChange) description: '3c'.
		
	]
]