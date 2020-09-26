-module(lagra_model).
-export([new_literal/1, new_literal/2]).
-export([new_literal_typed/2]).
-export([new_iri/1]).
-export([new_bnode/0]).
-export([literal_value/1]).
-export([literal_locale/1]).
-export([literal_type/1]).
-export([iri_to_text/1]).
-export([new_triple/3]).
-export([new_quad/4]).
-export([subject/1, predicate/1, object/1, graph/1]).
-export([quad_to_triple/1]).

-export([is_absolute_iri/1]).

-type literal() :: {string, binary()}            % untyped string
                 | {string, binary(), binary()}  % string plus locale
                 | {typed, term(), binary()}.    % typed non-string term

-type iri() :: {iri, binary()}.
-type bnode() :: {bnode, binary()}.
-type literalnode() :: {literal, literal()}.
-type resource() :: iri() | bnode().
-type rdfnode() :: resource() | literalnode().

-type subject() :: resource().
-type predicate() :: iri().
-type object() :: rdfnode().
-type graph() :: resource().

-type triple() :: {subject(), predicate(), object()}.
-type quad() :: {subject(), predicate(), object(), graph()}.

-type triple_pattern() :: {subject() | '_', predicate() | '_', object() | '_'}.
-type quad_pattern() :: {subject() | '_', predicate() | '_', object() | '_',
						 graph() | '_'}.
-type pattern() :: triple_pattern() | quad_pattern().

-export_type([literal/0, iri/0, bnode/0, literalnode/0, resource/0]).
-export_type([subject/0, predicate/0, object/0, graph/0]).
-export_type([rdfnode/0, triple/0, quad/0]).
-export_type([pattern/0, triple_pattern/0, quad_pattern/0]).

-type prefix_map() :: #{binary() | rdfnode() => unicode:string()}.

-export_type([prefix_map/0]).

-spec new_literal(term()) -> literalnode().
new_literal(Text) when is_binary(Text) ->
	{literal, {string, Text}};
new_literal(Text) when is_list(Text) ->
	{literal, {string, unicode:characters_to_binary(Text)}};
new_literal(Int) when is_integer(Int) ->
	{literal, {typed, Int, <<"http://www.w3.org/2001/XMLSchema#integer">>}};
new_literal(Float) when is_float(Float) ->
	{literal, {typed, Float, <<"http://www.w3.org/2001/XMLSchema#double">>}};
new_literal(Bool) when is_boolean(Bool) ->
	{literal, {typed, Bool, <<"http://www.w3.org/2001/XMLSchema#boolean">>}};
new_literal({{Y, M, D}, {H, N, S}})
  when is_integer(Y), is_integer(M), is_integer(D),
	   is_integer(H), is_integer(N), is_integer(S) ->
	DateString = unicode:characters_to_binary(
				   io_lib:format(
					 "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
					 [Y, M, D, H, N, S])),
	{literal, {typed, DateString, <<"http://www.w3.org/2001/XMLSchema#dateTime">>}}.

-spec new_literal(unicode:chardata(), unicode:chardata()) -> literalnode().
new_literal(Text, Locale) ->
	{literal, {string,
			   unicode:characters_to_binary(Text),
			   unicode:characters_to_binary(Locale)}}.

-spec new_literal_typed(term(), unicode:chardata()) -> literalnode().
new_literal_typed(Value, Type) ->
	{literal, {typed, Value, unicode:characters_to_binary(Type)}}.

-spec new_iri(unicode:chardata()) -> iri().
new_iri(String) ->
	{iri, unicode:characters_to_binary(String)}.

-spec new_bnode() -> bnode().
new_bnode() ->
	lagra_bnode:new().

-spec literal_value(literal() | literalnode()) -> term().
literal_value({literal, Lit}) ->
	literal_value(Lit);
literal_value({string, Text}) ->
	Text;
literal_value({string, Text, _}) ->
	Text;
literal_value({typed, Text, <<"http://www.w3.org/2001/XMLSchema#dateTime">>}) ->
	<<Y:4/binary, "-", M:2/binary, "-", D:2/binary, "T",
	  H:2/binary, ":", N:2/binary, ":", S:2/binary, "Z">> = Text,
	{{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
	 {binary_to_integer(H), binary_to_integer(N), binary_to_integer(S)}};
literal_value({typed, Text, _}) ->
	Text.

-spec literal_locale(literal() | literalnode()) -> string().
literal_locale({literal, Lit}) ->
	literal_locale(Lit);
literal_locale({string, _, Locale}) ->
	Locale.

-spec literal_type(literal() | literalnode()) -> string().
literal_type({literal, Lit}) ->
	literal_type(Lit);
literal_type({string, _}) ->
	<<"http://www.w3.org/2001/XMLSchema#string">>;
literal_type({string, _, _}) ->
	<<"http://www.w3.org/2001/XMLSchema#string">>;
literal_type({typed, _, Type}) ->
	Type.

-spec iri_to_text(iri()) -> string().
iri_to_text({iri, Location}) ->
	Location.

-spec new_triple(subject(), predicate(), object()) -> triple().
new_triple(S, P, O) ->
	{S, P, O}.

-spec new_quad(subject(), predicate(), object(), graph()) -> quad().
new_quad(S, P, O, G) ->
	{S, P, O, G}.

-spec subject(triple() | quad()) -> subject().
subject({S, _P, _O}) ->
	S;
subject({S, _P, _O, _G}) ->
	S.

-spec predicate(triple() | quad()) -> predicate().
predicate({_S, P, _O}) ->
	P;
predicate({_S, P, _O, _G}) ->
	P.

-spec object(triple() | quad()) -> object().
object({_S, _P, O}) ->
	O;
object({_S, _P, O, _G}) ->
	O.

-spec graph(triple() | quad()) -> graph().
graph({_S, _P, _O}) ->
	"";
graph({_S, _P, _O, G}) ->
	G.

-spec quad_to_triple(quad()) -> triple().
quad_to_triple({S, P, O, _}) ->
	{S, P, O}.

-spec is_absolute_iri(iri()) -> true | false.
is_absolute_iri({iri, Text}) ->
	case re:run(Text, "^[A-Za-z][A-Za-z0-9+.-]*:", [unicode]) of
		nomatch ->
			false;
		{match, _} ->
			true
	end.
