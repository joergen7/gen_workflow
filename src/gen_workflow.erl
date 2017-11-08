%% -*- erlang -*-
%%
%% A behavior for specifying data analysis applications in Erlang.
%%
%% Copyright 2017 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.0
%% @copyright 2017 Jörgen Brandt
%% -------------------------------------------------------------------

-module( gen_workflow ).
-behaviour( cre_client ).
-include_lib( "gen_workflow.hrl" ).


%%====================================================================
%% Exports
%%====================================================================

-export( [run_expr/2, run_module/2] ).
-export( [init/1, is_value/2, recv/4, step/2] ).


%%====================================================================
%% Callback definitions
%%====================================================================

-callback query() -> e().


%%====================================================================
%% API functions
%%====================================================================

-spec run_module( Mod, NWrk ) -> e()
when Mod  :: atom(),
     NWrk :: pos_integer().

run_module( Mod, NWrk )
when is_atom( Mod ) ->
  run_expr( Mod:query(), NWrk ).


-spec run_expr( E, NWrk ) -> e()
when E    :: e(),
     NWrk :: pos_integer().

run_expr( E, NWrk )
when is_integer( NWrk ), NWrk > 0 ->

  % start CRE application
  ok = cre:start(),

  % start Cuneiform workers
  ok = cf_worker:start(),
  
  % determine CRE master pid
  {ok, CreName} = cre:pid( node() ),

  % start CRE client
  {ok, Client} = cre_client:start_link( CreName, ?MODULE, [] ),

  Result = cre_client:eval( Client, E ),

  % stop CRE client
  ok = cre_client:stop( Client ),

  % stop Cuneiform workers
  ok = application:stop( cf_worker ),

  % stop CRE application
  ok = application:stop( cre ),

  Result.


%%====================================================================
%% CRE client callback implementations
%%====================================================================

-spec init( ClientArg :: _ ) -> [].

init( _ClientArg ) -> [].


-spec is_value( E :: e(), UsrInfo :: _ ) -> boolean().

is_value( E, _ ) ->
  gen_workflow_sem:is_value( E ).


-spec recv( E, A, Delta, UsrInfo ) -> e()
when E       :: e(),
     A       :: e(),
     Delta   :: e(),
     UsrInfo :: _.

recv( E, A, Delta, _UsrInfo ) ->
  gen_workflow_sem:subst_fut( E, A, Delta ).


-spec step( E, UsrInfo ) -> Result
when E       :: e(),
     UsrInfo :: _,
     Result  :: {ok, e()}
              | {ok_send, e(), e()}
              | norule.


step( E, _UsrInfo ) ->
  case gen_workflow_sem:find_context( E, hole ) of

    {ok, E, Ctx} ->
      E1 = gen_workflow_sem:reduce( E ),
      E2 = gen_workflow_sem:in_hole( E1, Ctx ),
      {ok, E2};

    no_ctx ->
      norule

  end.


