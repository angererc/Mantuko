Definitions.

NEWLINE		= (\n\r|\r|\n)
LINECOMMENT = (\;[^\n]*)
WHITESPACE  = ([\000-\s]|\t)

LETTER   	= [A-Za-z]
DIGIT   	= [0-9]

IDCHAR  	= ({LETTER}|{DIGIT}|[!$%*/\<\>\?~_\&\`\+\-])
IDENTIFIER	= {LETTER}({IDCHAR})*

NUMBER		= {DIGIT}({DIGIT})*

Rules.

{NEWLINE}		: skip_token.
{WHITESPACE}+  	: skip_token.
{LINECOMMENT}	: skip_token.

%check registers and symbols first, so that we can allow %array etc
\%{IDENTIFIER}  : {token, {reg_ident, TokenLine, TokenChars}}.
\'{IDENTIFIER}  : {token, {sym_ident, TokenLine, TokenChars}}.

\:		: {token,{colon,TokenLine}}.
\,		: {token,{comma,TokenLine}}.
\=		: {token,{equals,TokenLine}}.
\(		: {token,{lparen,TokenLine}}.
\)		: {token,{rparen,TokenLine}}.
\{		: {token,{lcurl,TokenLine}}.
\}		: {token,{rcurl,TokenLine}}.
\[		: {token,{lsquare,TokenLine}}.
\]		: {token,{rsquare,TokenLine}}.

this	: {token,{this,TokenLine}}.
new		: {token,{new,TokenLine}}.
struct	: {token,{struct,TokenLine}}.
array	: {token,{array,TokenLine}}.
lock	: {token,{lock,TokenLine}}.
nil		: {token,{nil, TokenLine}}.
now		: {token,{now, TokenLine}}.
sched	: {token,{sched, TokenLine}}.
\#block : {token,{act_block, TokenLine}}.
\#struct : {token,{act_struct, TokenLine}}.


%keywords
-> : {token,{arrow,TokenLine}}.

{IDENTIFIER}  : {token, {ident, TokenLine, TokenChars}}.
{NUMBER}  : {token, {num, TokenLine, TokenChars}}.

. : {error, lists:flatten(io_lib:format("bad input '~s' in line ~w", [TokenChars, TokenLine]))}.

Erlang code.
