-module (parser_tests).

-include_lib ("eunit/include/eunit.hrl").
-include("include/values.hrl").

assembler_syntax_test() ->
	Loader = loader:load_from_string("
	othermacro: {}
	somemacro: {
		%reg = %reg
		this['foo] = :othermacro
	}
	{
		;loads
		%reg = %reg
		%reg = 1
		%reg = 'sym
		%reg = {
			;other block
		}
		%reg = this
		%reg = new struct
		%reg = new array
		%reg = new lock
		%reg= nil
		%reg =now
		%reg=%reg[%reg]
		%reg = %reg[1]
		%reg = %reg['sym]
		%reg = this[%reg]
		%reg = this[1]
		%reg = this['sym]
		
		;those should work
		%array = this['array]
		%struct = this['struct]
		%new = this['new]
		%this = this['this]
		%block = this['block]
		%lock = this['lock]
		%sched = this['sched]
		
		%reg = now#block
		%reg = now#struct
		%reg = %reg#block
		%reg = %reg#struct
		%reg = this['foo]#struct
		%reg = this['foo]#block
		
		%reg = :somemacro
		%reg = sched :somemacro(this)
		%reg = sched :somemacro(new struct)
		
		;store variant 1
		%reg[%reg] = %reg['foo]
		%reg[%reg] = 1
		%reg[%reg] = 'sym
		%reg[%reg] = {
			;other block
		}
		%reg[%reg] = this[42]
		%reg[%reg] = new struct
		%reg[%reg] = new array
		%reg[%reg] = new lock
		%reg[%reg] = nil
		%reg[%reg] = now
		
		;store variant 2
		%reg['sym] = %reg[42]
		%reg['sym] = 1
		%reg['sym] = 'sym
		%reg['sym] = {
			;other block
		}
		%reg[1] = this[%reg]
		%reg[1] = new struct
		%reg[1] = new array
		%reg[1] = new lock
		%reg[1] = nil
		%reg[1] = now
		
		;store variant 3
		this[%reg] = %reg
		this[%reg] = 1
		this[%reg] = 'sym
		this[%reg] = {
			;other block
		}
		this[%reg] = this
		this[%reg] = new struct
		this[%reg] = new array
		this[%reg] = new lock
		this[%reg] = nil
		this[%reg] = now
		
		;schedule
		%reg -> %reg
		this['foo] -> %reg['bar]
		
		;primitives
		prim()
		prim('one)
		prim('one, %reg[%two])
		%reg = prim(this['one])
		%reg, %reg = prim(1, 2, 'three, %four)
		%reg, %reg['foo], %this = blubb(1, 2)
		
		;activations variant 1
		sched %reg(%reg)
		sched %reg(new struct)
		sched {}(%reg)
		sched {}(new struct)
		sched :othermacro(new struct)
		sched this['foo](this['bar])
		
		;activations variant 2
		%reg = sched %reg(%reg)
		%reg = sched %reg(new struct)
		%reg = sched {}(%reg)
		%reg = sched {}(new struct)
	}"),
	true = loader:contains_block({main}, Loader),
	true = loader:contains_block(othermacro, Loader),
	true = loader:contains_block(somemacro, Loader),
	false = loader:contains_block(xxxxx, Loader),
	ok.
	
	