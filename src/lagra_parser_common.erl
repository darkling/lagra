-module(lagra_parser_common).

-export([replace_quoted_string/1]).
-export([replace_quoted_iri/1]).
-export([replace_quoted_local/1]).

-export([fixup_float_text/1]).

-export([binary_to_type/2]).

-spec replace_quoted_string(unicode:chardata()) -> unicode:chardata().
replace_quoted_string(Text) ->
	[Head | Parts] = string:split(Text, "\\", all),
	Replaced = [string_replacer(P) || P <- fold_empty(Parts)],
	<<_/binary>> = unicode:characters_to_binary([Head | Replaced]).

-spec replace_quoted_iri(unicode:chardata()) -> unicode:chardata().
replace_quoted_iri(Text) ->
	replace_quoted(Text, fun iri_replacer/1).

-spec replace_quoted_local(unicode:chardata()) -> unicode:chardata().
replace_quoted_local(Text) ->
	replace_quoted(Text, fun local_replacer/1).

-spec replace_quoted(unicode:chardata(),
					 fun ((unicode:chardata()) -> unicode:chardata())) ->
							unicode:chardata().
replace_quoted(Text, Replacer) ->
	[Head | Parts] = string:split(Text, "\\", all),
	Replaced = [Replacer(P) || P <- Parts],
	<<_/binary>> = unicode:characters_to_binary([Head | Replaced]).

% Turn each adjacent pair of empty strings in the list into one empty string.
% This has the effect of turning \\ in the replace_quoted input into \
fold_empty([]) ->
	[];
fold_empty(["", "" | Tail]) ->
	["" | fold_empty(Tail)];
fold_empty([H|Tail]) ->
	[H | fold_empty(Tail)].

-spec iri_replacer(unicode:chardata()) -> unicode:chardata().
iri_replacer(<<$u, X1, X2, X3, X4, Tail/binary>>) ->
	<<_/binary>> = unicode:characters_to_binary(
					 [list_to_integer([X1, X2, X3, X4], 16), Tail]);
iri_replacer(<<$U, X1, X2, X3, X4, X5, X6, X7, X8, Tail/binary>>) ->
	<<_/binary>> = unicode:characters_to_binary(
					 [list_to_integer([X1, X2, X3, X4, X5, X6, X7, X8], 16),
					  Tail]);
iri_replacer(Text) ->
	<<_/binary>> = unicode:characters_to_binary([$\\, Text]).

-spec local_replacer(binary()) -> binary().
local_replacer(<<C, Text/binary>>)
  when C =:= $~; C =:= $.; C =:= $-; C =:= $!; C =:= $$; C =:= $&;
	   C =:= $'; C =:= $(; C =:= $); C =:= $*; C =:= $+; C =:= $,;
	   C =:= $;; C =:= $=; C =:= $/; C =:= $?; C =:= $#; C =:= $@;
	   C =:= $%; C =:= $_ ->
	<<C, Text/binary>>;
local_replacer(Text) when is_binary(Text) ->
	<<$\\, Text/binary>>.

-spec string_replacer(unicode:chardata()) -> binary().
string_replacer(<<$u, X1, X2, X3, X4, Tail/binary>>) ->
	unicode:characters_to_binary(
	  [list_to_integer([X1, X2, X3, X4], 16), Tail]);
string_replacer(<<$U, X1, X2, X3, X4, X5, X6, X7, X8, Tail/binary>>) ->
	unicode:characters_to_binary(
	  [list_to_integer([X1, X2, X3, X4, X5, X6, X7, X8], 16), Tail]);
string_replacer(<<$t, Text/binary>>) -> % Tab
	<<"\t", Text/binary>>;
string_replacer(<<$b, Text/binary>>) -> % Backspace
	<<8, Text/binary>>;
string_replacer(<<$n, Text/binary>>) -> % Line feed
	<<"\n", Text/binary>>;
string_replacer(<<$r, Text/binary>>) -> % Carriage return
	<<"\r", Text/binary>>;
string_replacer(<<$f, Text/binary>>) -> % Form feed
	<<12, Text/binary>>;
string_replacer(<<$", Text/binary>>) ->
	<<"\"", Text/binary>>;
string_replacer(<<$', Text/binary>>) ->
	<<"'", Text/binary>>;
string_replacer([]) ->
	<<"\\">>;
string_replacer(Text) ->
	%% The character following a backslash is quoted directly
	Text.

-spec fixup_float_text(unicode:chardata()) -> unicode:chardata().
fixup_float_text(Text) ->
	case string:lexemes(Text, "eE") of
		[M, E] ->
			<<(fixup_decimal_part(M))/binary, "e", E/binary>>;
		[M] ->
			fixup_decimal_part(M)
	end.

-spec fixup_decimal_part(unicode:chardata()) -> unicode:chardata().
fixup_decimal_part(M) ->
	case string:lexemes(M, ".") of
		[I] ->
			<<I/binary, ".0">>;
		[I, ""] ->
			<<I/binary, ".0">>;
		["", J] ->
			<<"0.", J/binary>>;
		[_I, _J] ->
			M
	end.

-spec binary_to_type(unicode:chardata(), unicode:chardata()) -> any().
binary_to_type(Text, <<"http://www.w3.org/2001/XMLSchema#", TypeStr/binary>>) ->
	convert_xsd_type(Text, TypeStr);
binary_to_type(Text, _) when is_binary(Text) ->
	Text.

-spec convert_xsd_type(unicode:chardata(), unicode:chardata()) -> any().
convert_xsd_type(<<"true">>, <<"boolean">>) ->
	true;
convert_xsd_type(<<"false">>, <<"boolean">>) ->
	false;
convert_xsd_type(Text, T)
  when T =:= <<"float">>;
	   T =:= <<"decimal">>;
	   T =:= <<"double">> ->
	binary_to_float(lagra_parser_common:fixup_float_text(Text));
convert_xsd_type(Text, T)
  when T =:= <<"integer">>; T =:= <<"nonPositiveInteger">>;
	   T =:= <<"negativeInteger">>; T =:= <<"long">>; T =:= <<"int">>;
	   T =:= <<"short">>; T =:= <<"byte">>; T =:= <<"nonNegativeInteger">>;
	   T =:= <<"unsignedLong">>; T =:= <<"unsignedInt">>;
	   T =:= <<"unsignedShort">>; T =:= <<"unsignedByte">>;
	   T =:= <<"positiveInteger">> ->
	binary_to_integer(Text);
convert_xsd_type(Text, _) ->
	Text.

%% TODO:
%%  base64Binary -> binary()
%%  hexBinary -> binary()
%%  dateTime -> {{Y, M, D}, {H, M, S}}
%%  & all their friends
