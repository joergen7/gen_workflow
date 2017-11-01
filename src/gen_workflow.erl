-module( gen_workflow ).


%%====================================================================
%% Exports
%%====================================================================

%% Type constructors
-export( [t_fn_arg/2] ).
-export( [t_str/0, t_file/0, t_bool/0, t_fn/3] ).

%% Expression constructors
-export( [lam_ntv_arg/3, app_arg/2] ).
-export( [str/1, file/1, true/0, false/0, cnd/3, var/1, lam_ntv/2, app/2] ).


%%====================================================================
%% Type definitions
%%====================================================================

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

-type e() :: {str, s()}
           | {file, s()}
           | true
           | false
           | {cnd, e(), e(), e()}
           | x
           | {lam, ntv, [lam_ntv_arg()], e()}
           | {app, e(), [app_arg()]}.


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
when is_atom( X ), X =/= true, X =/= false,
     is_list( S ) ->
  {X, S, T}.


-spec app_arg( S :: s(), E :: e() ) -> app_arg().

app_arg( S, E ) when is_list( S ) ->
  {S, E}.


-spec str( S :: s() ) -> e().

str( S ) when is_list( S ) ->
  {str, S}.


-spec file( S :: s() ) -> e().

file( S ) when is_list( S ) ->
  {file, S}.


-spec true() -> e().

true() ->
  true.


-spec false() -> e().

false() ->
  false.


-spec cnd( EIf :: e(), EThen :: e(), EElse :: e() ) -> e().

cnd( EIf, EThen, EElse ) ->
  {cnd, EIf, EThen, EElse}.
  

-spec var( X :: x() ) -> e().

var( X )
when is_atom( X ), X =/= true, X =/= false ->
  X.


-spec lam_ntv( ArgLst :: [lam_ntv_arg()], EBody :: e() ) -> e().

lam_ntv( ArgLst, EBody ) when is_list( ArgLst ) ->
  {lam, ntv, ArgLst, EBody}.


-spec app( F :: e(), ArgLst :: [app_arg()] ) -> e().

app( F, ArgLst ) when is_list( ArgLst ) ->
  {app, F, ArgLst}.