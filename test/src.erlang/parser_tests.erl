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
		%reg = new
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
		%reg = fork :somemacro(new)
		
		;store variant 1
		%reg[%reg] = %reg['foo]
		%reg[%reg] = 1
		%reg[%reg] = 'sym
		%reg[%reg] = {
			;other block
		}
		%reg[%reg] = this[42]
		%reg[%reg] = new
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
		%reg[1] = new
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
		this[%reg] = new
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
		fork %reg(new)
		fork {}(%reg)
		fork {}(new)
		fork :othermacro(new)
		fork this['foo](this['bar])
		
		;activations variant 2
		%reg = fork %reg(%reg)
		%reg = fork %reg(new)
		%reg = fork {}(%reg)
		%reg = fork {}(new)
	}"),
	true = loader:contains_block({main}, Repository),
	true = loader:contains_block(othermacro, Repository),
	true = loader:contains_block(somemacro, Repository),
	false = loader:contains_block(xxxxx, Repository),
	ok.
	
	