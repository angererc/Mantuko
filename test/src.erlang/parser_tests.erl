-module (parser_tests).

-include_lib ("eunit/include/eunit.hrl").
-include("include/values.hrl").

assembler_syntax_test() ->
	Repository = loader:load_from_string("
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
		%reg = struct
		%reg = array
		%reg = lock
		%reg= nil
		%reg =now
		%reg=%reg[%reg]
		%reg = %reg[1]
		%reg = %reg['sym]
		%reg = this[%reg]
		%reg = this[1]
		%reg = this['sym]
		
		%reg = now#block
		%reg = now#struct
		%reg = %reg#block
		%reg = %reg#struct
		%reg = this['foo]#struct
		%reg = this['foo]#block
		
		%reg = :somemacro
		%reg = fork :somemacro(this)
		%reg = fork :somemacro(struct)
		
		;store variant 1
		%reg[%reg] = %reg['foo]
		%reg[%reg] = 1
		%reg[%reg] = 'sym
		%reg[%reg] = {
			;other block
		}
		%reg[%reg] = this[42]
		%reg[%reg] = struct
		%reg[%reg] = array
		%reg[%reg] = lock
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
		%reg[1] = struct
		%reg[1] = array
		%reg[1] = lock
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
		this[%reg] = struct
		this[%reg] = array
		this[%reg] = lock
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
		
		;activations variant 1
		fork %reg(%reg)
		fork %reg(struct)
		fork {}(%reg)
		fork {}(struct)
		fork :othermacro(struct)
		fork this['foo](this['bar])
		
		;activations variant 2
		%reg = fork %reg(%reg)
		%reg = fork %reg(struct)
		%reg = fork {}(%reg)
		%reg = fork {}(struct)
	}"),
	true = loader:contains_block({main}, Repository),
	true = loader:contains_block(othermacro, Repository),
	true = loader:contains_block(somemacro, Repository),
	false = loader:contains_block(xxxxx, Repository),
	ok.
	
	