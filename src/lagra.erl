-module(lagra).

-export([create_store/1]).
-export([destroy_store/1]).
-export([po_from_s/2]).
-export([o_from_sp/3]).

-type store() :: pid().

-export_type([store/0]).

-spec create_store(atom()) -> store().
create_store(trivial) ->
	{ok, Child} = supervisor:start_child(lagra_store_trivial_sup, []),
	Child.

%-spec create_store(atom(), proplists:proplist()) -> store().

-spec destroy_store(store()) -> term().
destroy_store(Store) ->
	supervisor:terminate_child(lagra_store_trivial_sup, Store).

-spec po_from_s(store(), lagra_model:subject()) ->
					   [{lagra_model:predicate(), lagra_model:object()}].
po_from_s(Store, Subject) ->
	gen_server:call(Store, {po_from_s, Subject}).

-spec o_from_sp(store(), lagra_model:subject(), lagra_model:predicate()) ->
					   [lagra_model:object()].
o_from_sp(Store, Subject, Predicate) ->
	gen_server:call(Store, {o_from_sp, Subject, Predicate}).
