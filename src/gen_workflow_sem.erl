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

-module( gen_workflow_sem ).
-include_lib( "gen_workflow.hrl" ).

%%====================================================================
%% Exports
%%====================================================================

-export( [reduce/1] ).
-export( [is_value/1] ).
-export( [rename/3, subst/3, subst_fut/3, gensym/1] ).
-export( [in_hole/2, find_context/1] ).


%%====================================================================
%% Notion of reduction
%%====================================================================

-spec reduce( E :: e() ) -> e().

reduce( {cnd, _, {true, _}, EThen, _} ) ->                     % E-true
  EThen;

reduce( {cnd, _, {false, _}, _, EElse} ) ->                    % E-false
  EElse;

reduce( {app, _, {lam_ntv, _, [], EBody}, []} ) ->             % E-beta-base
  EBody;

reduce( {app, AppInfo,                                         % E-beta
              {lam_ntv, LamInfo, [{X, S, _}|LamArgTl], EBody},
              [{S, E}|AppArgTl]} ) ->
  EBody1 = subst( EBody, X, E ),
  EFn1 = {lam_ntv, LamInfo, LamArgTl, EBody1},
  {app, AppInfo, EFn1, AppArgTl}.
  

%%====================================================================
%% Determining values
%%====================================================================

-spec is_value( E :: e() ) -> boolean().

is_value( {str, _, _} )        -> true;
is_value( {file, _, _} )       -> true;
is_value( {true, _} )          -> true;
is_value( {false, _} )         -> true;
is_value( {cnd, _, _, _, _} )  -> false;
is_value( {var, _, _} )        -> false;
is_value( {lam_ntv, _, _, _} ) -> true;
is_value( {app, _, _, _} )     -> false.


%%====================================================================
%% Substitution and renaming
%%====================================================================

-spec rename( E, X1, X2 ) -> e()
when E  :: e(),
     X1 :: x(),
     X2 :: x().

rename( E = {str, _, _}, _, _ )  -> E;
rename( E = {file, _, _}, _, _ ) -> E;
rename( E = {true, _}, _, _ )    -> E;
rename( E = {false, _}, _, _ )   -> E;

rename( {cnd, Info, EIf, EThen, EElse}, X, Y ) ->
  {cnd, Info, rename( EIf, X, Y ),
              rename( EThen, X, Y ),
              rename( EElse, X, Y )};

rename( {var, Info, X}, X, Y ) ->
  {var, Info, Y};

rename( E = {var, _, _}, _, _ )  -> E;

rename( {lam_ntv, Info, ArgLst, EBody}, X, Y ) ->

  F = fun
        ( {X1, S1, T1} ) when X1 =:= X -> {Y, S1, T1};
        ( Arg )                        -> Arg
      end,

  ArgLst1 = [F( Arg ) || Arg <- ArgLst],
  EBody1 = rename( EBody, X, Y ),

  {lam_ntv, Info, ArgLst1, EBody1};

rename( {app, Info, EFn, ArgLst}, X, Y ) ->

  EFn1 = rename( EFn, X, Y ),
  ArgLst1 = [{S, rename( E, X, Y )} || {S, E} <- ArgLst],

  {app, Info, EFn1, ArgLst1}.


-spec subst( E1, X, E2 ) -> e()
when E1 :: e(),
     X  :: x(),
     E2 :: e().

subst( E1 = {str, _, _}, _, _ )  -> E1;
subst( E1 = {file, _, _}, _, _ ) -> E1;
subst( E1 = {true, _}, _, _ )    -> E1;
subst( E1 = {false, _}, _, _ )   -> E1;

subst( {cnd, Info, EIf, EThen, EElse}, X, E2 ) ->
  {cnd, Info, subst( EIf, X, E2 ),
              subst( EThen, X, E2 ),
              subst( EElse, X, E2 )};

subst( {var, _, X}, X, E2 )      -> E2;
subst( E1 = {var, _, _}, _, _ )  -> E1;

subst( {lam_ntv, Info, ArgLst, EBody}, X, E2 ) ->

  F = fun( {X1, S, T}, {lam_ntv, Info1, ArgLst1, EBody1} ) ->
        X2 = gensym( X1 ),
        EBody2 = rename( EBody1, X1, X2 ),
        {lam_ntv, Info1, [{X2, S, T}|ArgLst1], EBody2}
      end,

  Lam0 = {lam_ntv, Info, [], EBody},
  {lam_ntv, Info, NewArgLst, NewEBody} = lists:foldr( F, Lam0, ArgLst ),

  {lam_ntv, Info, NewArgLst, subst( NewEBody, X, E2 )};

subst( {app, Info, EFn, ArgLst}, X, E2 ) ->

  EFn1 = subst( EFn, X, E2 ),
  ArgLst1 = [{S, subst( E, X, E2 )} || {S, E} <- ArgLst],

  {app, Info, EFn1, ArgLst1}.

-spec subst_fut( E, A, Delta ) -> e()
when E     :: e(),
     A     :: e(),
     Delta :: e().

subst_fut( _, _, _ ) -> error( nyi ).


-spec gensym( X :: atom() ) -> atom().

gensym( X ) when is_atom( X ) ->
  [S1|_] = string:tokens( atom_to_list( X ), "$" ),
  N = erlang:unique_integer( [positive, monotonic] ),
  S2 = [$$|integer_to_list( N )],
  list_to_atom( S1++S2 ).

%%====================================================================
%% Evaluation contexts
%%====================================================================

-spec in_hole( E , Ctx ) -> e()
when E   :: e(),
     Ctx :: ctx().

in_hole( E, hole ) ->
  E;

in_hole( E, {cnd, Info, EIf, EThen, EElse} ) ->
  % note that we do not traverse the then- and else expressions because there
  % can never be a hole down these two roads
  {cnd, Info, in_hole( E, EIf ), EThen, EElse};

% TODO: first check for foreignness
% in_hole( E, {app, Info, {lam_frn, ...}, ArgLst} ) -> ...

in_hole( E, {app, Info, EFn, ArgLst} ) ->
  % note that we do not traverse the argument list because unless the function
  % expression is a foreign function, the hole must be left hand
  {app, Info, in_hole( E, EFn ), ArgLst}.


-spec find_context( E :: e() ) -> {ok, e(), ctx()} | no_ctx.

find_context( E ) ->
  try try_context( E, hole ) of
    no_ctx -> no_ctx
  catch
    throw:{E1, Ctx1} -> {ok, E1, Ctx1}
  end.


-spec try_context( E, Ctx ) -> no_ctx
when E   :: e(),
     Ctx :: ctx().

try_context( {str, _, _}, _ )        -> no_ctx;
try_context( {file, _, _}, _ )       -> no_ctx;
try_context( {true, _}, _ )          -> no_ctx;
try_context( {false, _}, _ )         -> no_ctx;

try_context( E = {cnd, Info, EIf, EThen, EElse}, Ctx ) ->
  case is_value( EIf ) of
    true  -> throw( {E, Ctx} );
    false ->
      Ctx1 = in_hole( {cnd, Info, hole, EThen, EElse}, Ctx ),
      try_context( EIf, Ctx1 )
  end;

try_context( {var, _, _}, _ )        -> no_ctx;
try_context( {lam_ntv, _, _, _}, _ ) -> no_ctx;

try_context( E = {app, _, {lam_ntv, _, _, _}, _}, Ctx ) ->
  throw( {E, Ctx} );

try_context( {app, Info, EFn, ArgLst}, Ctx ) ->
  Ctx1 = in_hole( {app, Info, hole, ArgLst}, Ctx ),
  try_context( EFn, Ctx1 ).


