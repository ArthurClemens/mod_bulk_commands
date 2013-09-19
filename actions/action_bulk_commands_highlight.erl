-module(action_bulk_commands_highlight).
-include("zotonic.hrl").

-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    % Pass all arguments
    Script = [<<"action_bulk_commands.highlight(">>, z_utils:js_object(Args, Context), $), $; ],
	{Script, Context}.