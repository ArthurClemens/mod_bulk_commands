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
    Json = z_context:get_q("data", Context),
    AnimationSpeed = z_context:get_q("animationSpeed", Context),
    JsonData = mochijson2:decode(Json),
    DoneAction = {delete_done, []},
    Actions = lists:foldl(
        fun({struct, [{_, HrefBin}, {_, RowIdBin}]}, Acc) ->
            Href = z_convert:to_list(HrefBin),
            PageId = z_convert:to_integer(re:replace(Href, "/admin/edit/", "", [{return, list}])),
            RowId = z_convert:to_list(RowIdBin),
            case m_rsc:exists(PageId, Context) of
                true ->
                    Action = case z_acl:rsc_deletable(PageId, Context) of
                        true ->
                            ok = m_rsc:delete(PageId, Context),
                            {remove, [{fadeout, true}, {speed, AnimationSpeed}, {target, RowId}]};
                        false ->
                            {growl, [{text, ?__(io_lib:format("You are not allowed to delete page ~p. It may be protected.", [PageId]), Context)}, {type, "error"}]}
                        end,
                    [Action | Acc];
                false -> 
                    Action = {growl, [{text, ?__("This page does no longer exist.", Context)}, {type, "error"}]},
                    [Action | Acc]         
                end
        end,
        [DoneAction],
        JsonData
    ),
    z_render:wire(Actions, Context);

observe_postback_notify(_, _Context) -> 
     undefined.