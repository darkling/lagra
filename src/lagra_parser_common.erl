-module(lagra_parser_common).

-export([replace_quoted/2]).
-export([iri/1]).
-export([local/1]).
-export([string/1]).

-export([fixup_float_text/1]).

-export([list_to_type/2]).

-spec replace_quoted(string(), fun ((string()) -> string())) -> string().
replace_quoted(Text, Replacer) ->
	[Head | Parts] = string:split(Text, "\\", all),
	Replaced = [Replacer(P) || P <- fold_empty(Parts)],
	Res = lists:flatten([Head | Replaced]),
	Res.

% Turn each adjacent pair of empty strings in the list into one empty string.
% This has the effect of turning \\ in the replace_quoted input into \
fold_empty([]) ->
	[];
fold_empty(["", "" | Tail]) ->
	["" | fold_empty(Tail)];
fold_empty([H|Tail]) ->
	[H | fold_empty(Tail)].

-spec iri(string()) -> string().
iri([$u, X1, X2, X3, X4 | Tail]) ->
	[list_to_integer([X1, X2, X3, X4], 16) | Tail];
iri([$U, X1, X2, X3, X4, X5, X6, X7, X8 | Tail]) ->
	[list_to_integer([X1, X2, X3, X4, X5, X6, X7, X8], 16) | Tail];
iri(Text) ->
	[$\\, Text].

-spec local(string()) -> string().
local([C|Text])
  when C =:= $~; C =:= $.; C =:= $-; C =:= $!; C =:= $$; C =:= $&;
	   C =:= $'; C =:= $(; C =:= $); C =:= $*; C =:= $+; C =:= $,;
	   C =:= $;; C =:= $=; C =:= $/; C =:= $?; C =:= $#; C =:= $@;
	   C =:= $%; C =:= $_ ->
	[C|Text];
local(Text) ->
	[$\\|Text].

-spec string(string()) -> string().
string([$u, X1, X2, X3, X4 | Tail]) ->
	[list_to_integer([X1, X2, X3, X4], 16) | Tail];
string([$U, X1, X2, X3, X4, X5, X6, X7, X8 | Tail]) ->
	[list_to_integer([X1, X2, X3, X4, X5, X6, X7, X8], 16) | Tail];
string([$t|Text]) -> % Tab
	["\t"|Text];
string([$b|Text]) -> % Backspace
	[8|Text];
string([$n|Text]) -> % Line feed
	["\n"|Text];
string([$r|Text]) -> % Carriage return
	["\r"|Text];
string([$f|Text]) -> % Form feed
	[12|Text];
string([$"|Text]) ->
	["\""|Text];
string([$'|Text]) ->
	["'"|Text];
string([]) ->
	"\\";
string(Text) ->
	%% The character following a backslash is quoted directly
	Text.

-spec fixup_float_text(string()) -> string().
fixup_float_text(Text) ->
	case string:lexemes(Text, "eE") of
		[M, E] ->
			fixup_decimal_part(M) ++ "e" ++ E;
		[M] ->
			fixup_decimal_part(M)
	end.

-spec fixup_decimal_part(string()) -> string().
fixup_decimal_part(M) ->
	case string:lexemes(M, ".") of
		[I] ->
			I++".0";
		[I, ""] ->
			I++".0";
		["", J] ->
			"0."++J;
		[_I, _J] ->
			M
	end.

-spec list_to_type(string(), string()) -> any().
list_to_type(Text, "http://www.w3.org/2001/XMLSchema#"++TypeStr) ->
	convert_xsd_type(Text, TypeStr);
list_to_type(Text, _) ->
	Text.

-spec convert_xsd_type(string(), string()) -> any().
convert_xsd_type("true", "boolean") ->
	true;
convert_xsd_type("false", "boolean") ->
	false;
convert_xsd_type(Text, T)
  when T =:= "float";
	   T =:= "decimal";
	   T =:= "double" ->
	list_to_float(lagra_parser_common:fixup_float_text(Text));
convert_xsd_type(Text, T)
  when T =:= "integer"; T =:= "nonPositiveInteger"; T =:= "negativeInteger";
	   T =:= "long"; T =:= "int"; T =:= "short"; T =:= "byte";
	   T =:= "nonNegativeInteger"; T =:= "unsignedLong"; T =:= "unsignedInt";
	   T =:= "unsignedShort"; T =:= "unsignedByte"; T =:= "positiveInteger" ->
	list_to_integer(Text);
convert_xsd_type(Text, _) ->
	Text.

%% TODO:
%%  base64Binary -> binary()
%%  hexBinary -> binary()
%%  dateTime -> {{Y, M, D}, {H, M, S}}
%%  & all their friends
