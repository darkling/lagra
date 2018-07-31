-module(lagra_model).
-export([new_literal/1, new_literal/2]).
-export([new_literal_typed/2]).
-export([new_iri/1]).
-export([new_bnode/1]).
-export([literal_value/1]).
-export([literal_locale/1]).
-export([literal_type/1]).
-export([new_triple/3]).
-export([new_quad/4]).
-export([subject/1, predicate/1, object/1, graph/1]).
-export([quad_to_triple/1]).

-export([is_absolute_iri/1]).

-type literal() :: {string, string()}            % untyped string
                 | {string, string(), string()}  % string plus locale
                 | {typed, term(), string()}.    % typed non-string term

-type iri() :: {iri, string()}.
-type bnode() :: {bnode, string()}.
-type literalnode() :: {literal, literal()}.
-type resource() :: iri() | bnode().
-type rdfnode() :: resource() | literalnode().

-type subject() :: resource().
-type predicate() :: iri().
-type object() :: rdfnode().
-type graph() :: iri().

-type triple() :: {subject(), predicate(), object()}.
-type quad() :: {subject(), predicate(), object(), graph()}.

-export_type([literal/0, iri/0, bnode/0, literalnode/0, resource/0]).
-export_type([subject/0, predicate/0, object/0, graph/0]).
-export_type([rdfnode/0, triple/0, quad/0]).

-spec new_literal(term()) -> literalnode().
new_literal(Text) when is_list(Text) ->
	{literal, {string, Text}};
new_literal(Int) when is_integer(Int) ->
	{literal, {typed, Int, "http://www.w3.org/2001/XMLSchema#integer"}};
new_literal(Float) when is_float(Float) ->
	{literal, {typed, Float, "http://www.w3.org/2001/XMLSchema#float"}}.

-spec new_literal(string(), string()) -> literalnode().
new_literal(Text, Locale) when is_list(Text), is_list(Locale) ->
	{literal, {string, Text, Locale}}.

-spec new_literal_typed(term(), string()) -> literalnode().
new_literal_typed(Value, Type) when is_list(Type) ->
	{literal, {typed, Value, Type}}.

-spec new_iri(string()) -> iri().
new_iri(String) when is_list(String) ->
	{iri, String}.

-spec new_bnode(lagra:store()) -> bnode().
new_bnode(Store) ->
	gen_server:call(Store, {new_bnode}).

-spec literal_value(literal() | literalnode()) -> term().
literal_value({literal, Lit}) ->
	literal_value(Lit);
literal_value({string, Text}) ->
	Text;
literal_value({string, Text, _}) ->
	Text;
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
	"xsd:string";
literal_type({string, _, _}) ->
	"xsd:string";
literal_type({typed, _, Type}) ->
	Type.

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
