%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc edts code to deal with distribution.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_dist).

%%%_* Exports ==================================================================

%% API
-export([ connect/1
        , connect_all/0
        , make_sname/1]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Pings Node registered with the local epmd, so that a connection is
%% established.
%% @end
-spec connect(Node::atom()) -> ok.
%%------------------------------------------------------------------------------
connect(Node) ->
  net_adm:ping(Node).

%%------------------------------------------------------------------------------
%% @doc
%% Pings all nodes registered with the local epmd, so that connections are
%% established.
%% @end
-spec connect_all() -> ok.
%%------------------------------------------------------------------------------
connect_all() ->
  {ok, Hostname} = inet:gethostname(),
  {ok, Nodes}    = net_adm:names(),
  lists:foreach(fun({Name, _Port}) ->
                    {ok, Nodename} = make_sname(Name, Hostname),
                    connect(Nodename)
                end,
                Nodes).

%%------------------------------------------------------------------------------
%% @doc
%% Converts a string to a valid erlang sname for localhost.
%% @end
-spec make_sname(string()) -> atom().
%%------------------------------------------------------------------------------
make_sname(Name) ->
  {ok, Hostname} = inet:gethostname(),
  make_sname(Name, Hostname).

%%------------------------------------------------------------------------------
%% @doc
%% Converts a string to a valid erlang sname for Hostname.
%% @end
-spec make_sname(Name::string(), Hostname::string()) -> atom().
%%------------------------------------------------------------------------------
make_sname(Name, Hostname) ->
  try
    {ok, list_to_atom(Name ++ "@" ++ Hostname)}
  catch
    _C:Reason -> {error, Reason}
  end.


%%%_* Internal functions =======================================================


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
