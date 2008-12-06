%%%-------------------------------------------------------------------
%%% File    : twitterl.erl
%%% Author  : Yomi (baphled boodah) Akindayini <yomi@boodah.net>
%%% Description : Gen server used to interact with twitter
%%%
%%% Created :  2 Dec 2008 by Yomi (baphled boodah) Akindayini  <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(twitterl).

-behaviour(gen_server).
-import(twitterl_interface).
%% API
%-export([start/0]).
-compile(export_all).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(twitterl, {sessions, base_url, delay, lastcall}).

start() ->
    inets:start(),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #twitterl{
       sessions = gb_trees:empty(),
       delay = 0,
       lastcall = calendar:datetime_to_gregorian_seconds(erlang:universaltime())}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

add_session(Login, Password) ->
    gen_server:call({global, ?MODULE}, {add_session, Login, Password}, infinity).

remove_session(Login) ->
    gen_server:call({global, ?MODULE}, {remove_session, Login}, infinity).

call(Client,Method) ->
    twitterl_server:call(Client, Method, []).
call(Client, Method, Args) ->
    gen_server:call({global, ?MODULE}, {Client, Method, Args}, infinity).

handle_call({add_session, Login, Password}, _From, State) ->
    NewTree =  case gb_trees:is_defined(Login, State#twitterl.sessions) of
        true ->
		       Result = "Already have a session",
		       State#twitterl.sessions;
        false -> 
		       Result = "Created session",
		       gb_trees:insert(Login, {Login, Password}, State#twitterl.sessions)
    end,
    {reply, Result, State#twitterl{ sessions = NewTree }};

handle_call({remove_session, Login}, _From, State) ->
    NewTree =  case gb_trees:is_defined(Login, State#twitterl.sessions) of
        true -> 
		       Result = "Session dropped",
		       gb_trees:delete(Login, State#twitterl.sessions);
        false -> 
		       Result = "Session not dropped",
		       State#twitterl.sessions
    end,
    {reply, Result, State#twitterl{ sessions = NewTree }};

handle_call({Client, Method, Args}, _From, State) ->
    Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    Response = case session_from_client(State, Client) of
        {error, Reason} -> {error, Reason};
        {Login, Password} ->
            try apply(twitterl_server, Method, [Login, Password, Args])
            catch
                Err:Msg ->
                    io:format("error: ~p:~p~n", [Err, Msg]),
                    {error, unsupported_method}
            end;
        _ -> {error, unknown}
    end,
    {reply, Response, State#twitterl{ lastcall = Now }};

handle_call(_, _From, State) -> {noreply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% private
session_from_client(State, Client) ->
    case gb_trees:is_defined(Client, State#twitterl.sessions) of
        false -> {error, invalid_client};
        true -> gb_trees:get(Client, State#twitterl.sessions)
    end.

status_trends(_Login, _Password, _Args) ->
    twitterl_interface:trends().
