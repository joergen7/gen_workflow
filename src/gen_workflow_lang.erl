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

-module( gen_workflow_lang ).


%%====================================================================
%% Exports
%%====================================================================

%% Type constructors
-export( [t_fn_arg/2] ).
-export( [t_str/0, t_file/0, t_bool/0, t_fn/3] ).

%% Expression constructors
-export( [lam_ntv_arg/3, app_arg/2] ).
-export( [str/1, file/1, true/0, false/0, cnd/3, var/1, lam_ntv/2, app/2] ).
-export( [str/2, file/2, true/1, false/1, cnd/4, var/2, lam_ntv/3, app/3] ).


%%====================================================================
%% Type definitions
%%====================================================================

-type info() :: na
              | {string(), pos_integer()}.

-type s() :: string().

-type x() :: atom().


-type tau() :: ntv
             | frn.

-type t_fn_arg() :: {s(), t()}.

-type t() :: 'Str'
           | 'File'
           | 'Bool'
           | {'Fn', tau(), [t_fn_arg()], t()}.


-type lam_ntv_arg() :: {x(), s(), t()}.

-type app_arg() :: {s(), e()}.

-type e() :: {str, info(), s()}
           | {file, info(), s()}
           | {true, info()}
           | {false, info()}
           | {cnd, info(), e(), e(), e()}
           | {var, info(), x()}
           | {lam_ntv, info(), [lam_ntv_arg()], e()}
           | {app, info(), e(), [app_arg()]}.


%%====================================================================
%% Type constructors
%%====================================================================

-spec t_fn_arg( S :: s(), T :: t() ) -> t_fn_arg().

t_fn_arg( S, T ) when is_list( S ) ->
  {S, T}.


-spec t_str() -> t().

t_str() ->
  'Str'.


-spec t_file() -> t().

t_file() ->
  'File'.


-spec t_bool() -> t().

t_bool() ->
  'Bool'.


-spec t_fn( Tau :: tau(), ArgLst :: [t_fn_arg()], TRet :: t() ) -> t().

t_fn( Tau, ArgLst, TRet )
when Tau =:= ntv orelse Tau =:= frn,
     is_list( ArgLst ) ->
  {'Fn', Tau, ArgLst, TRet}.


%%====================================================================
%% Expression constructors
%%====================================================================

-spec lam_ntv_arg( X :: x(), S :: s(), T :: t() ) -> lam_ntv_arg().

lam_ntv_arg( X, S, T )
when is_atom( X ),
     is_list( S ) ->
  {X, S, T}.


-spec app_arg( S :: s(), E :: e() ) -> app_arg().

app_arg( S, E ) when is_list( S ) ->
  {S, E}.


-spec str( S :: s() ) -> e().

str( S ) ->
  str( na, S ).


-spec str( Info :: info(), S :: s() ) -> e().

str( Info, S )
when Info =:= na orelse is_tuple( Info ),
     is_list( S ) ->
  {str, Info, S}.


-spec file( S :: s() ) -> e().

file( S ) ->
  file( na, S ).


-spec file( Info :: info(), S :: s() ) -> e().

file( Info, S )
when Info =:= na orelse is_tuple( Info ),
     is_list( S ) ->
  {file, Info, S}.


-spec true() -> e().

true() ->
  true( na ).


-spec true( Info :: info() ) -> e().

true( Info )
when Info =:= na orelse is_tuple( Info ) ->
  {true, Info}.


-spec false() -> e().

false() ->
  false( na ).


-spec false( Info :: info() ) -> e().

false( Info )
when Info =:= na orelse is_tuple( Info ) ->
  {false, Info}.


-spec cnd( EIf :: e(), EThen :: e(), EElse :: e() ) -> e().

cnd( EIf, EThen, EElse ) ->
  cnd( na, EIf, EThen, EElse ).


-spec cnd( Info :: info(), EIf :: e(), EThen :: e(), EElse :: e() ) -> e().

cnd( Info, EIf, EThen, EElse )
when Info =:= na orelse is_tuple( Info ) ->
  {cnd, Info, EIf, EThen, EElse}.
  

-spec var( X :: x() ) -> e().

var( X ) ->
  var( na, X ).


-spec var( Info :: info(), X :: x() ) -> e().

var( Info, X )
when Info =:= na orelse is_tuple( Info ),
     is_atom( X ) ->
  {var, Info, X}.


-spec lam_ntv( ArgLst :: [lam_ntv_arg()], EBody :: e() ) -> e().

lam_ntv( ArgLst, EBody ) ->
  lam_ntv( na, ArgLst, EBody ).


-spec lam_ntv( Info :: info(), ArgLst :: [lam_ntv_arg()], EBody :: e() ) -> e().

lam_ntv( Info, ArgLst, EBody )
when Info =:= na orelse is_tuple( Info ),
     is_list( ArgLst ) ->
  {lam_ntv, Info, ArgLst, EBody}.


-spec app( F :: e(), ArgLst :: [app_arg()] ) -> e().

app( F, ArgLst ) ->
  app( na, F, ArgLst ).


-spec app( Info :: info(), F :: e(), ArgLst :: [app_arg()] ) -> e().

app( Info, F, ArgLst )
when Info =:= na orelse is_tuple( Info ),
     is_list( ArgLst ) ->
  {app, Info, F, ArgLst}.