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


%%====================================================================
%% Exports
%%====================================================================

-export( [init/1, is_value/2, recv/4, step/2] ).
-export( [reduce/1] ).
-export( [subst/3, in_hole/2, find_context/2, subst_fut/3] ).


%%====================================================================
%% Type definitions
%%====================================================================

-type ctx() :: hole
             | {cnd, info(), ctx(), e(), e()}
             | {app, info(), ctx(), [app_arg()]}.


%%====================================================================
%% CRE client callback functions
%%====================================================================

-spec init( ClientArg :: _ ) -> [].

init( _ClientArg ) -> [].


-spec is_value( E :: e(), UsrInfo :: _ ) -> boolean().

is_value( {str, _, _}, _ )        -> true;
is_value( {file, _, _}, _ )       -> true;
is_value( {true, _}, _ )          -> true;
is_value( {false, _}, _ )         -> true;
is_value( {cnd, _, _, _, _}, _ )  -> false;
is_value( {var, _, _}, _ )        -> false;
is_value( {lam_ntv, _, _, _}, _ ) -> true;
is_value( {app, _, _, _}, _ )     -> false.


-spec recv( E, A, Delta, UsrInfo ) -> e()
when E       :: e(),
     A       :: e(),
     Delta   :: e(),
     UsrInfo :: _.

recv( E, A, Delta, _UsrInfo ) ->
  subst_fut( E, A, Delta ).


-spec step( E, UsrInfo ) -> {ok, e()} | {ok_send, e(), e()} | norule
when E       :: e(),
     UsrInfo :: _.

step( E, _UsrInfo ) ->
  case find_context( E, hole ) of
    {ok, E, Ctx} -> {ok, in_hole( reduce( E ), Ctx )};
    norule       -> norule
  end.


%%====================================================================
%% Notion of reduction
%%====================================================================

-spec reduce( E :: e() ) -> e().

reduce( {cnd, _, {true, _}, EThen, _} ) ->
  EThen;

reduce( {cnd, _, {false, _}, _, EElse} ) ->
  EElse;

reduce( {app, _, {lam_ntv, _, [], EBody}, []} ) ->
  EBody;

reduce( {app, AppInfo,
              {lam_ntv, LamInfo, [{X, S, T}|LamArgTl], EBody},
              [{S, E}|AppArgTl]} ) ->
  EBody1 = subst( EBody, X, E ),
  EFn1 = {lam_ntv, LamInfo, LamArgTl, EBody1},
  {app, AppInfo, EFn1, AppArgTl}.
  

%%====================================================================
%% Helper functions
%%====================================================================

-spec subst( E1, X, E2 ) -> e()
when E1 :: e(),
     X  :: x(),
     E2 :: e().

subst( _, _, _ ) -> error( nyi ).


-spec in_hole( E , Ctx ) -> e()
when E   :: e(),
     Ctx :: ctx().

in_hole( E, hole ) ->
  E;

in_hole( E, {cnd, Info, EIf, EThen, EElse} ) ->
  {cnd, Info, in_hole( E, EIf ), EThen, EElse};

in_hole( E, {app, Info, EFn, ArgLst} ) ->
  {app, Info, in_hole( E, EFn), ArgLst}.


-spec find_context( E, Ctx ) -> {ok, e(), ctx()} | no_ctx.
when E   :: e(),
     Ctx :: ctx().

find_context( E, Ctx ) ->
  try try_context( E, Ctx ) of
    no_ctx -> no_ctx
  catch
    throw:{E1, Ctx1} -> {ok, E1, Ctx1}
  end.


-spec subst_fut( E, A, Delta ) -> e()
when E     :: e(),
     A     :: e(),
     Delta :: e().

subst_fut( _, _, _ ) -> error( nyi ).

%%====================================================================
%% Internal functions
%%====================================================================

-spec try_context( E, Ctx ) -> no_ctx
when E   :: e(),
     Ctx :: ctx().

try_context( {str, _, _}, _ )        -> no_ctx;
try_context( {file, _, _}, _ )       -> no_ctx;
try_context( {true, _}, _ )          -> no_ctx;
try_context( {false, _}, _ )         -> no_ctx;

try_context( E = {cnd, _, {true, _}, _, _}, Ctx ) ->
  throw( {E, Ctx} );

try_context( E = {cnd, _, {false, _}, _, _}, Ctx ) ->
  throw( {E, Ctx} );

try_context( {cnd, Info, EIf, EThen, EElse}, Ctx ) ->
  Ctx1 = in_hole( {cnd, Info, hole, EThen, EElse}, Ctx ),
  try_context( EIf, Ctx1 );

try_context( {var, _, _}, _ )        -> no_ctx;
try_context( {lam_ntv, _, _, _}, _ ) -> no_ctx;

try_context( E = {app, _, {lam_ntv, _, _, _}, _}, Ctx ) ->
  throw( {E, Ctx} );

try_context( {app, Info, EFn, ArgLst}, Ctx ) ->
  Ctx1 = in_hole( {app, Info, hole, ArgLst}, Ctx ),
  try_context( EFn, Ctx1 ).


