
-module(edts_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(EDTS_PORT, 4587).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    DispatchFile   = filename:join(code:priv_dir(edts), "dispatch.conf"),
    {ok, Dispatch} = file:consult(DispatchFile),

    WebmConf = [
                {port, ?EDTS_PORT},
                {dispatch, Dispatch}
               ],
    Webmachine = {webmachine_mochiweb,
                  {webmachine_mochiweb, start, [WebmConf]},
                  permanent, 5000, worker, [webmachine_mochiweb]},
    Edts = {edts_server,
            {edts_server, start_link, []},
            permanent, 5000, worker, [edts_server]},
    Children = [Edts, Webmachine],
    {ok, { {one_for_one, 5, 10}, Children} }.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
