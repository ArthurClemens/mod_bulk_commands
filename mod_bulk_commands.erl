%% @author Arthur Clemens
%% @copyright 2013 Arthur Clemens
%% Generated on 2013-09-07
%% @doc Bulk admin commands

-module(mod_bulk_commands).
-author("Arthur Clemens").

-mod_title("Bulk admin commands").
-mod_description("Adds bulk commands to admin data tables. Currently only delete is implemented.").
-mod_prio(100).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    observe_postback_notify/2
]).

observe_postback_notify(#postback_notify{message="bulk_delete"}, Context) ->
    bulk_commands(delete, [], Context);
observe_postback_notify(#postback_notify{message="published_true"}, Context) ->
    bulk_commands(published, [{state, true}], Context);
observe_postback_notify(#postback_notify{message="published_false"}, Context) ->
    bulk_commands(published, [{state, false}], Context);
observe_postback_notify(_, _Context) -> 
     undefined.
     
bulk_commands(Command, Args, Context) ->
    Json = z_context:get_q("data", Context),
    JsonData = mochijson2:decode(Json),
    AnimationSpeed = z_context:get_q("animationSpeed", Context),
    
    Actions = lists:foldl(
        fun({struct, [{_, HrefBin}, {_, RowIdBin}]}, Acc) ->
            Href = z_convert:to_list(HrefBin),
            PageId = z_convert:to_integer(re:replace(Href, "/admin/edit/", "", [{return, list}])),
            RowId = z_convert:to_list(RowIdBin),
            case m_rsc:exists(PageId, Context) of
                true ->
                    Args1 = lists:append(Args, [
                        {pageId, PageId},
                        {rowId, RowId},
                        {animationSpeed, AnimationSpeed}
                    ]),
                    Action = action_command(Command, Args1, Context),
                    [Action | Acc];
                false -> 
                    Action = {growl, [{text, ?__("This page does no longer exist.", Context)}, {type, "error"}]},
                    [Action | Acc]         
                end
        end,
        [done_action(Command, Context)],
        JsonData
    ),
    z_render:wire(Actions, Context).

done_action(Command, Context) ->
    AnimationSpeed = z_context:get_q("animationSpeed", Context),
    TableId = z_context:get_q("tableId", Context),
    {done, [
        {command, Command},
        {tableId, TableId},
        {animationSpeed, AnimationSpeed}
    ]}.

action_command(Command, Args, Context) when Command == delete ->
    PageId = proplists:get_value(pageId, Args),
    case z_acl:rsc_deletable(PageId, Context) of
        true ->
            AnimationSpeed = proplists:get_value(animationSpeed, Args),
            RowId = proplists:get_value(rowId, Args),
            ok = m_rsc:delete(PageId, Context),
            {remove, [{fadeout, true}, {speed, AnimationSpeed}, {target, RowId}]};
        false ->
            {growl, [{text, ?__(io_lib:format("You are not allowed to delete page ~p. It may be protected.", [PageId]), Context)}, {type, "error"}]}
        end;

action_command(Command, Args, Context) when Command == published ->
    PageId = proplists:get_value(pageId, Args),
    case z_acl:rsc_editable(PageId, Context) of
        true ->
            State = proplists:get_value(state, Args),
            AnimationSpeed = proplists:get_value(animationSpeed, Args),
            RowId = proplists:get_value(rowId, Args),
            Props = [
                {is_published, State}
            ],
            lager:warning("is_published=~p", [bool_to_integer(State)]),
            m_rsc:update(PageId, Props, Context),
            {highlight, [{speed, AnimationSpeed}, {rowId, RowId}, {is_published, bool_to_integer(State)}]};
        false ->
            {growl, [{text, ?__(io_lib:format("You are not allowed to change this property of page ~p.", [PageId]), Context)}, {type, "error"}]}
        end;

action_command(_, _, Context) ->
    Context.
    
    
bool_to_integer(V) when V == true ->
    1;
bool_to_integer(V) when V == false ->
    0;
bool_to_integer(_) ->
    0.