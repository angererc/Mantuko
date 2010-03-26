% @type block(Name, Filename, StartLine, EndLine, Body)
%	Name = atom()
%	Filename = string()
%	StartLine = integer()
%	EndLine = integer()
%	Body = [instruction()].	
% The id is the macro name or 'main' for the main block. The id is on position
% 2 of the tuple so that one can search a list of blocks using lists:keysearch/3
% Example: <pre>{ *blockbody* }</pre>

-record (block, {
				name::atom(), 
				filename::string(), 
				start_line::non_neg_integer(), 
				end_line::non_neg_integer(), 
				body::[term()]}).
				