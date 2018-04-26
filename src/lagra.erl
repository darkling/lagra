-module(lagra).

-export([create_store/1, create_store/2]).
-export([destroy_store/1]).

-export([parse/3, parse/4]).

-export([add/2]).

-export([o_from_sp/3]).
-export([p_from_so/3]).
-export([s_from_po/3]).
-export([po_from_s/2]).
%-export([so_from_p/2]).
%-export([sp_from_o/2]).

%-export([o_from_spg/4]).
%-export([p_from_sog/4]).
%-export([s_from_pog/4]).
%-export([po_from_sg/3]).
%-export([so_from_pg/3]).
%-export([sp_from_og/3]).

-type store() :: pid().

-export_type([store/0]).

-spec create_store(atom()) -> store().
create_store(trivial) ->
	create_store(trivial, []).

-spec create_store(atom(), proplists:proplist()) -> store().
create_store(trivial, _Options) ->
	{ok, Child} = supervisor:start_child(lagra_store_trivial_sup, []),
	Child.

-spec destroy_store(store()) -> term().
destroy_store(Store) ->
	supervisor:terminate_child(lagra_store_trivial_sup, Store).

-spec parse(store(), file:io_device(), atom(), proplists:proplist())
		   -> ok | {error, term(), integer()}.
parse(Store, File, ntriples, _Options) ->
	lagra_parser_ntriples:parse(Store, File, []).

-spec parse(store(), file:io_device(), atom())
		   -> ok | {error, term(), integer()}.
parse(Store, File, Parser) ->
	parse(Store, File, Parser, []).

-spec add(store(), lagra_model:triple() | lagra_model:quad())
		 -> ok | {error, term()}.
add(Store, Triple) when tuple_size(Triple) =:= 3 ->
	gen_server:call(Store, {add_triple, Triple});
add(Store, Quad) when tuple_size(Quad) =:= 4 ->
	gen_server:call(Store, {add_quad, Quad}).

-spec o_from_sp(store(), lagra_model:subject(), lagra_model:predicate())
			   -> [lagra_model:object()].
o_from_sp(Store, Subject, Predicate) ->
	gen_server:call(Store, {o_from_sp, Subject, Predicate}).

-spec p_from_so(store(), lagra_model:subject(), lagra_model:object())
			   -> [lagra_model:predicate()].
p_from_so(Store, Subject, Object) ->
	gen_server:call(Store, {p_from_so, Subject, Object}).

-spec s_from_po(store(), lagra_model:predicate(), lagra_model:object())
			   -> [lagra_model:subject()].
s_from_po(Store, Predicate, Object) ->
	gen_server:call(Store, {s_from_po, Predicate, Object}).


-spec po_from_s(store(), lagra_model:subject())
			   -> [{lagra_model:predicate(), lagra_model:object()}].
po_from_s(Store, Subject) ->
	gen_server:call(Store, {po_from_s, Subject}).
