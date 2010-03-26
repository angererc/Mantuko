% *****************************
% instructions
% *****************************

% @type instruction() = load() | store() | schedule() | intrinsic() | activation().

% @type move(LineNo, Target, Value)
%	LineNo = integer()
%	Target = values:reg() | values:slot()
%	Value = values:compiletime_value().
% Example: <pre>%reg = 42</pre> and <pre>%reg[%reg] = fork %foo(new)</pre>
-record (move, {line_no::non_neg_integer(), target, value}).

% @type order(LineNo, Lhs, Rhs)
%	LineNo = integer()
%	Lhs = values:reg() | values:slot()
%	Rhs = values:reg() | values:slot().
% creates a new edge in the schedule.
% Node: a schedule: now -> %reg doesn't make sense because that's only legal iff there is an
% edge between now and the activation in %reg already.
% Example: <pre>%reg1 -> %reg2</pre>.
-record (order, {line_no::non_neg_integer(), lhs, rhs}). 

% @type intrinsic(LineNo, Name, OutLHSides, InValues)
%	LineNo = integer()
%	Name = atom()
%	InValues = [values:compiletime_value()]
%	OutLHSides = [values:reg()|values:slot()].
% calls the intrinsic function 'Name', passing the InValues and writing back the result(s) into
% the OutRegisters. InValues and OutRegisters can be empty lists.
% Note, that the number and types of required in-values and the number and types of returned
% values are defined by the intrinsic function.
% Example: <pre>%reg1, %reg2 = somefun 42, 'sym, %reg</pre>
-record (intrinsic, {line_no::non_neg_integer(), nth, name, out_lhsides, in_values}). %primitive operations, in_values=[value]

% @type schedule(LineNo, Nth, Target, Block, Struct)
% 	LineNo = integer()
%	Nth = integer()
%	Target = values:reg() | values:slot() | undefined
%	Block = values:reg() | values:block_ref()
%	Struct = values:reg() | values:new() | values:this().
% Creates a new activation and schedules it after now(). The new activation can be assigned to a register so
% that it can be used somewhere else (e.g., stored in a struct); if the activation is not stored in a register,
% Target is undefined
% Example: <pre>%reg = %blockreg(new)</pre>
-record (schedule, {line_no::non_neg_integer(), nth, target, block, struct}). %target=register|undefined, block=register|block, struct=register|new|this