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


\'		: {token,{tick,TokenLine}}.
\:		: {token,{colon,TokenLine}}.
\,		: {token,{comma,TokenLine}}.
\=		: {token,{equals,TokenLine}}.
\(		: {token,{lparen,TokenLine}}.
\)		: {token,{rparen,TokenLine}}.
\{		: {token,{lcurl,TokenLine}}.
\}		: {token,{rcurl,TokenLine}}.
\[		: {token,{lsquare,TokenLine}}.
\]		: {token,{rsquare,TokenLine}}.
\%		: {token,{percent,TokenLine}}. %used for registers

this	: {token,{this,TokenLine}}.
struct	: {token,{struct,TokenLine}}.
array	: {token,{array,TokenLine}}.
lock	: {token,{lock,TokenLine}}.
nil		: {token,{nil, TokenLine}}.
now		: {token,{now, TokenLine}}.
fork	: {token,{fork, TokenLine}}.
\#block : {token,{act_block, TokenLine}}.
\#struct : {token,{act_struct, TokenLine}}.


%keywords
-> : {token,{schedule,TokenLine}}.

{IDENTIFIER}  : {token, {ident, TokenLine, TokenChars}}.
{NUMBER}  : {token, {num, TokenLine, TokenChars}}.

. : {error, lists:flatten(io_lib:format("bad input '~s' in line ~w", [TokenChars, TokenLine]))}.

Erlang code.
