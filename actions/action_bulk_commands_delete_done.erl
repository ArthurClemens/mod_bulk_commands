-module(action_bulk_commands_delete_done).
-include("zotonic.hrl").

-export([render_action/4]).

render_action(_TriggerId, _TargetId, _Args, Context) -> 
    Script = [<<"action_bulk_commands.onDeleteDone();">>],
	{Script, Context}.