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

-module( gen_workflow_sem_test ).
-include_lib( "eunit/include/eunit.hrl" ).

%%====================================================================
%% Imports
%%====================================================================

-import( gen_workflow_sem, [reduce/1] ).
-import( gen_workflow_sem, [is_value/1, rename/3, subst/3, gensym/1] ).
-import( gen_workflow_sem, [in_hole/2, find_context/1] ).

-import( gen_workflow_lang, [t_str/0, t_file/0, t_bool/0, t_fn/3] ).
-import( gen_workflow_lang, [lam_ntv_arg/3, app_arg/2] ).
-import( gen_workflow_lang, [str/1, file/1, true/0, false/0, cnd/3, var/1,
                             lam_ntv/2, app/2] ).
 
%%====================================================================
%% Expression definitions
%%====================================================================

e_lam1() ->
  lam_ntv( [lam_ntv_arg( x, "x", t_str() ),
            lam_ntv_arg( y, "y", t_file() )], var( x ) ).

e_lam_first() ->
  lam_ntv( [lam_ntv_arg( x, "x", t_str() ),
            lam_ntv_arg( y, "y", t_str() )], var( x ) ).

e_lam_const() ->
  lam_ntv( [], str( "blub" ) ).

e_lam_id() ->
  lam_ntv( [lam_ntv_arg( x, "x", t_str() )], var( x ) ).

e_app_id() ->
  app( e_lam_id(), [app_arg( "x", str( "blub" ) )] ).

%%====================================================================
%% Notion of reduction
%%====================================================================

reduce_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"condition with true if expression reduces to then expression",
     fun cnd_with_true_if_expr_reduces_to_then_expr/0},

    {"condition with false if expression reduces to then expression",
     fun cnd_with_false_if_expr_reduces_to_then_expr/0},

    {"native application without arguments reduces to lambda body",
     fun nat_app_without_arg_reduces_to_lam_body/0},

    {"native application with single argument reduces to empty application",
     fun nat_app_with_single_arg_reduces_to_empty_application/0}
   ]
  }.

cnd_with_true_if_expr_reduces_to_then_expr() ->
  ETrue = str( "blub" ),
  E1 = cnd( true(), ETrue, str( "bla" ) ),
  ?assertEqual( ETrue, reduce( E1 ) ).

cnd_with_false_if_expr_reduces_to_then_expr() ->
  EFalse = str( "bla" ),
  E1 = cnd( false(), str( "blub" ), EFalse ),
  ?assertEqual( EFalse, reduce( E1 ) ).

nat_app_without_arg_reduces_to_lam_body() ->
  E1 = app( e_lam_const(), [] ),
  E2 = str( "blub" ),
  ?assertEqual( E2, reduce( E1 ) ).

nat_app_with_single_arg_reduces_to_empty_application() ->
  E1 = e_app_id(),
  E2 = app( lam_ntv( [], str( "blub") ), [] ),
  ?assertEqual( E2, reduce( E1 ) ).


%%====================================================================
%% Determining values
%%====================================================================

is_value_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"str is value",            fun str_is_value/0},
    {"file is value",           fun file_is_value/0},
    {"true is value",           fun true_is_value/0},
    {"false is value",          fun false_is_value/0},
    {"condition is no value",   fun cnd_is_no_value/0},
    {"variable is no value",    fun variable_is_no_value/0},
    {"native lambda is value",  fun lam_ntv_is_value/0},
    {"application is no value", fun app_is_no_value/0}
   ]
  }.

str_is_value() ->
  ?assert( is_value( str( "blub" ) ) ).

file_is_value() ->
  ?assert( is_value( file( "blub" ) ) ).

true_is_value() ->
  ?assert( is_value( true() ) ).

false_is_value() ->
  ?assert( is_value( false() ) ).

cnd_is_no_value() ->
  ?assertNot( is_value( cnd( true(), true(), false() ) ) ).

variable_is_no_value() ->
  ?assertNot( is_value( var( x ) ) ).

lam_ntv_is_value() ->
  ?assert( is_value( e_lam1() ) ),
  ?assert( is_value( e_lam_first() ) ),
  ?assert( is_value( e_lam_const() ) ),
  ?assert( is_value( e_lam_id() ) ).

app_is_no_value() ->
  ?assertNot( is_value( e_app_id() ) ),
  ?assertNot( is_value( app( e_lam_const(), [] ) ) ).


%%====================================================================
%% Substitution and renaming
%%====================================================================

rename_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"rename leaves str alone",   fun rename_leaves_str_alone/0},
    {"rename leaves file alone",  fun rename_leaves_file_alone/0},
    {"rename leaves true alone",  fun rename_leaves_true_alone/0},
    {"rename leaves false alone", fun rename_leaves_false_alone/0},

    {"rename propagates to condition if expression",
     fun rename_propagates_to_cnd_if_expr/0},

    {"rename propagates to condition then expression",
     fun rename_propagates_to_cnd_then_expr/0},

    {"rename propagates to condition else expression",
     fun rename_propagates_to_cnd_else_expr/0},

    {"rename leaves non-matching variable alone",
     fun rename_leaves_nonmatching_var_alone/0},

    {"matching variable is renamed",
     fun matching_var_is_renamed/0},

    {"rename propagates to native lambda argument binding",
     fun rename_propagates_to_lam_ntv_arg_lst/0},

    {"rename leaves non-matching lambda argument alone",
     fun rename_leaves_nonmatching_lam_ntv_arg_alone/0},

    {"rename propagates to native lambda body",
     fun rename_propagates_to_lam_ntv_body/0},

    {"rename propagates to application function",
     fun rename_propagates_to_app_function/0},

    {"rename propagates to application arguments",
     fun rename_propagates_to_app_arg_lst/0}
   ]
  }.


rename_leaves_str_alone() ->
  E = str( "blub" ),
  ?assertEqual( E, rename( E, x, y ) ).

rename_leaves_file_alone() ->
  E = file( "blub" ),
  ?assertEqual( E, rename( E, x, y ) ).

rename_leaves_true_alone() ->
  E = true(),
  ?assertEqual( E, rename( E, x, y ) ).

rename_leaves_false_alone() ->
  E = false(),
  ?assertEqual( E, rename( E, x, y ) ).

rename_propagates_to_cnd_if_expr() ->
  E1 = cnd( var( x ), true(), false() ),
  E2 = cnd( var( y ), true(), false() ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_cnd_then_expr() ->
  E1 = cnd( true(), var( x ), false() ),
  E2 = cnd( true(), var( y ), false() ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_cnd_else_expr() ->
  E1 = cnd( true(), true(), var( x ) ),
  E2 = cnd( true(), true(), var( y ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_leaves_nonmatching_var_alone() ->
  E = var( x ),
  ?assertEqual( E, rename( E, y, z ) ).

matching_var_is_renamed() ->
  E1 = var( x ),
  E2 = var( y ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_lam_ntv_arg_lst() ->
  E1 = lam_ntv( [lam_ntv_arg( x, "x", t_str() )], str( "blub" ) ),
  E2 = lam_ntv( [lam_ntv_arg( y, "x", t_str() )], str( "blub" ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_leaves_nonmatching_lam_ntv_arg_alone() ->
  E = lam_ntv( [lam_ntv_arg( z, "z", t_str() )], str( "blub" ) ),
  ?assertEqual( E, rename( E, x, y ) ).

rename_propagates_to_lam_ntv_body() ->
  E1 = lam_ntv( [], var( x ) ),
  E2 = lam_ntv( [], var( y ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_app_function() ->
  E1 = app( var( x ), [] ),
  E2 = app( var( y ), [] ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_app_arg_lst() ->
  E1 = app( var( f ), [app_arg( "x", var( x ) )] ),
  E2 = app( var( f ), [app_arg( "x", var( y ) )] ),
  ?assertEqual( E2, rename( E1, x, y ) ).


subst_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"substitution leaves str alone",   fun subst_leaves_str_alone/0},
    {"substitution leaves file alone",  fun subst_leaves_file_alone/0},
    {"substitution leaves true alone",  fun subst_leaves_true_alone/0},
    {"substitution leaves false alone", fun subst_leaves_false_alone/0},

    {"substitution propagates to condition if expression",
     fun subst_propagates_to_cnd_if_expr/0},

    {"substitution propagates to condition then expression",
     fun subst_propagates_to_cnd_then_expr/0},

    {"substitution propagates to condition else expression",
     fun subst_propagates_to_cnd_else_expr/0},

    {"substitution leaves non-matching variable alone",
     fun subst_leaves_nonmatching_var_alone/0},

    {"matching variable is substituted",
     fun matching_var_is_substituted/0},

    {"substitution propagates to native lambda body",
     fun subst_propagates_to_lam_ntv_body/0},

    {"substitution is shadowed by bound variables",
     fun subst_shadowed_by_bound_var/0},

    {"substitution is capture avoiding",
     fun subst_is_capture_avoiding/0},

    {"substitution retains order of native lambda arguments",
     fun subst_retains_order_of_lam_ntv_arg_lst/0},

    {"substitution propagates to application function",
     fun subst_propagates_to_app_function/0},

    {"substitution propagates to application arguments",
     fun subst_propagates_to_app_arg_lst/0}
   ]
  }.


subst_leaves_str_alone() ->
  E = str( "blub" ),
  ?assertEqual( E, subst( E, x, var( y ) ) ).

subst_leaves_file_alone() ->
  E = file( "blub" ),
  ?assertEqual( E, subst( E, x, var( y ) ) ).

subst_leaves_true_alone() ->
  E = true(),
  ?assertEqual( E, subst( E, x, var( y ) ) ).

subst_leaves_false_alone() ->
  E = false(),
  ?assertEqual( E, subst( E, x, var( y ) ) ).

subst_propagates_to_cnd_if_expr() ->
  E1 = cnd( var( x ), true(), false() ),
  E2 = cnd( var( y ), true(), false() ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_propagates_to_cnd_then_expr() ->
  E1 = cnd( true(), var( x ), false() ),
  E2 = cnd( true(), var( y ), false() ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_propagates_to_cnd_else_expr() ->
  E1 = cnd( true(), true(), var( x ) ),
  E2 = cnd( true(), true(), var( y ) ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_leaves_nonmatching_var_alone() ->
  E = var( x ),
  ?assertEqual( E, subst( E, y, var( z ) ) ).

matching_var_is_substituted() ->
  E1 = var( x ),
  E2 = var( y ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_propagates_to_lam_ntv_body() ->
  E1 = lam_ntv( [], var( x ) ),
  E2 = lam_ntv( [], var( y ) ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_shadowed_by_bound_var() ->
  E = lam_ntv( [lam_ntv_arg( x, "x", t_str() )], var( x ) ),
  ?assertMatch( {lam_ntv, na, [{X, "x", 'Str'}], {var, na, X}},
                subst( E, x, var( y ) ) ).

subst_is_capture_avoiding() ->
  E = lam_ntv( [lam_ntv_arg( x, "x", t_str() )], var( y ) ),
  ?assertNotMatch( {lam_ntv, na, [{X, "x", 'Str'}], {var, na, X}},
                   subst( E, y, var( x ) ) ).

subst_retains_order_of_lam_ntv_arg_lst() ->
  ?assertMatch( {lam_ntv, na, [{X, "x", 'Str'},
                               {_, "y", 'File'}], {var, na, X}},
                subst( e_lam1(), a, var( b ) ) ).

subst_propagates_to_app_function() ->
  E1 = app( var( x ), [] ),
  E2 = app( var( y ), [] ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_propagates_to_app_arg_lst() ->
  E1 = app( var( f ), [app_arg( "x", var( x ) )] ),
  E2 = app( var( f ), [app_arg( "x", var( y ) )] ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).


gensym_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"gensym adds unique number to var",
     fun gensym_adds_unique_number_to_var/0},

    {"gensym replaces unique number instead of appending",
     fun gensym_replaces_unique_number_instead_of_appending/0}
   ]
  }.

gensym_adds_unique_number_to_var() ->
  X = gensym( blub ),
  [A, _] = string:tokens( atom_to_list( X ), "$" ),
  ?assertEqual( "blub", A ).

gensym_replaces_unique_number_instead_of_appending() ->
  X1 = gensym( blub ),
  X2 = gensym( X1 ),
  [A, _] = string:tokens( atom_to_list( X2 ), "$" ),
  ?assertEqual( "blub", A ).

%%====================================================================
%% Evaluation contexts
%%====================================================================

in_hole_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"inserting in the empty context returns the original expression",
     fun insert_in_empty_ctx_returns_original_expr/0},

    {"inserting traverses conditional's if expression",
     fun insert_traverses_cnd_if_expr/0},

    {"inserting traverses non-foreign application's function position",
     fun insert_traverses_non_frn_app_fun_pos/0}
   ]
  }.

insert_in_empty_ctx_returns_original_expr() ->
  E = str( "blub" ),
  ?assertEqual( E, in_hole( E, hole ) ).

insert_traverses_cnd_if_expr() ->
  E1 = true(),
  Ctx = cnd( hole, str( "bla" ), str( "blub" ) ),
  E2 = cnd( true(), str( "bla" ), str( "blub" ) ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

insert_traverses_non_frn_app_fun_pos() ->
  E1 = var( f ),
  Ctx = app( hole, [] ),
  E2 = app( var( f ), [] ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

find_context_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"string string is no redex",
     fun string_is_no_redex/0},

    {"file is no redex",
     fun file_is_no_redex/0},
      
    {"true is no redex",
     fun true_is_no_redex/0},
      
    {"false is no redex",
     fun false_is_no_redex/0},

    {"conditional with true if expression is redex",
     fun cnd_with_true_if_expr_is_redex/0},

    {"conditional with false if expression is redex",
     fun cnd_with_false_if_expr_is_redex/0},

    {"find_context traverses conditional's if expression",
     fun find_context_traverses_cnd_if_expr/0},

    {"var is no redex",
     fun var_is_no_redex/0},

    {"native function is no redex",
     fun lam_ntv_is_no_redex/0},

    {"application with native function is redex",
     fun app_with_lam_ntv_is_redex/0},

    {"find_context traverses application's function position",
     fun find_context_traverses_app_fn_pos/0}

   ]}.
    

string_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( str( "blub" ) ) ).

file_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( file( "blub" ) ) ).

true_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( true() ) ).

false_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( false() ) ).

cnd_with_true_if_expr_is_redex() ->
  E = cnd( true(), str( "bla" ), str( "blub" ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

cnd_with_false_if_expr_is_redex() ->
  E = cnd( false(), str( "bla" ), str( "blub" ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_cnd_if_expr() ->
  E = cnd( true(), true(), false() ),
  Ctx = cnd( hole, str( "bla" ), str( "blub") ),
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

var_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( var( x ) ) ).

lam_ntv_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( e_lam1() ) ).

app_with_lam_ntv_is_redex() ->
  E = e_app_id(),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_app_fn_pos() ->
  E = cnd( true(), e_lam_const(), e_lam_const() ),
  Ctx = app( hole, [] ),
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).
