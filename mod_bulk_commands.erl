%% @author Arthur Clemens <arthur@visiblearea.com>
%% @copyright 2013 Arthur Clemens
%% Date: 2013-12-25
%% @doc Admin add-on to delete or change the state of multiple items at once.

-module(mod_bulk_commands).
-author("Arthur Clemens <arthur@visiblearea.com>").

-mod_title("Bulk admin commands").
-mod_description("Admin add-on to delete or change the state of multiple items at once.").
-mod_prio(500).
-mod_depends([admin]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    init/1,
    observe_postback_notify/2
]).

%% Set the config value which pages to add bulk commands to
%% This is a comma-separated list of context variables "selected"
init(Context) ->
    PagesConfigSet = m_config:get(?MODULE, pages, Context),
    case PagesConfigSet of
        undefined -> m_config:set_value(?MODULE, pages, "admin_overview_rsc,admin_media", Context);
        _ -> undefined
    end,
    CommandsConfigSet = m_config:get(?MODULE, commands, Context),
    case CommandsConfigSet of
        undefined -> m_config:set_value(?MODULE, commands, "delete,published,featured,protected", Context);
        _ -> undefined
    end,
	ok.

observe_postback_notify(#postback_notify{message="bulk_delete"}, Context) ->
    bulk_commands(delete, [], Context);
observe_postback_notify(#postback_notify{message="published_true"}, Context) ->
    bulk_commands(is_published, [{state, true}], Context);
observe_postback_notify(#postback_notify{message="published_false"}, Context) ->
    bulk_commands(is_published, [{state, false}], Context);
observe_postback_notify(#postback_notify{message="featured_true"}, Context) ->
    bulk_commands(is_featured, [{state, true}], Context);
observe_postback_notify(#postback_notify{message="featured_false"}, Context) ->
    bulk_commands(is_featured, [{state, false}], Context);
observe_postback_notify(#postback_notify{message="protected_true"}, Context) ->
    bulk_commands(is_protected, [{state, true}], Context);
observe_postback_notify(#postback_notify{message="protected_false"}, Context) ->
    bulk_commands(is_protected, [{state, false}], Context);
observe_postback_notify(_, _Context) ->
     undefined.

bulk_commands(Command, Args, Context) ->
    Json = z_context:get_q("data", Context),
    JsonData = mochijson2:decode(Json),

    {ok, Pattern} = re:compile("(\\d+)"),
    Actions = lists:foldl(
        fun({struct, [{_, HrefBin}, {_, RowIdBin}]}, Acc) ->
            Href = z_convert:to_list(HrefBin),
            case (re:run(Href, Pattern, [{capture, first, list}])) of
                {match,[PageIdStr]} ->
                    PageId = z_convert:to_integer(PageIdStr),
                    RowId = z_convert:to_list(RowIdBin),
                    case m_rsc:exists(PageId, Context) of
                        true ->
                            Args1 = lists:append(Args, [
                                {pageId, PageId},
                                {rowId, RowId}
                            ]),
                            Action = action_command(Command, Args1, Context),
                            [Action | Acc];
                        false ->
                            Action = {growl, [{text, ?__("This page does no longer exist.", Context)}, {type, "error"}]},
                            [Action | Acc]
                        end;
                nomatch -> undefined
            end
        end,
        [done_action(Command, Context)],
        JsonData
    ),
    z_render:wire(Actions, Context).

done_action(Command, Context) ->
    TableId = z_context:get_q("tableId", Context),
    {done, [
        {command, Command},
        {tableId, TableId}
    ]}.

action_command(Command, Args, Context) when Command == delete ->
    PageId = proplists:get_value(pageId, Args),
    case z_acl:rsc_deletable(PageId, Context) of
        true ->
            RowId = proplists:get_value(rowId, Args),
            ok = m_rsc:delete(PageId, Context),
            {remove, [{fadeout, true}, {speed, 200}, {target, RowId}]};
        false ->
            {growl, [{text, ?__(io_lib:format("You are not allowed to delete page ~p. It may be protected.", [PageId]), Context)}, {type, "error"}]}
        end;

action_command(Command, Args, Context) when Command == is_published ->
    change_state(Command, Args, Context);

action_command(Command, Args, Context) when Command == is_featured ->
    change_state(Command, Args, Context);

action_command(Command, Args, Context) when Command == is_protected ->
    change_state(Command, Args, Context);

action_command(_, _, Context) ->
    Context.

change_state(Command, Args, Context) ->
    PageId = proplists:get_value(pageId, Args),
    case z_acl:rsc_editable(PageId, Context) of
        true ->
            State = proplists:get_value(state, Args),
            RowId = proplists:get_value(rowId, Args),
            Props = [
                {Command, State}
            ],
            m_rsc:update(PageId, Props, Context),
            {highlight, [{speed, 200}, {rowId, RowId}, {Command, bool_to_integer(State)}]};
        false ->
            {growl, [{text, ?__(io_lib:format("You are not allowed to change this property of page ~p.", [PageId]), Context)}, {type, "error"}]}
        end.


bool_to_integer(V) when V == true ->
    1;
bool_to_integer(V) when V == false ->
    0;
bool_to_integer(_) ->
    0.
