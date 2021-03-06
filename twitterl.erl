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
-export([start/0,call/2,call/3,add_session/2,remove_session/1]).
%% Wrapper methods, exposed to show commands useable with call
-export([find_trends/3,find_tweets/3,find_term/3,tweet_timeline/3,my_timeline/3]).

%Status based methods
-export([status_show/3]).

%User based methods
-export([user_show/3,user_followers/3,user_friends/3,user_timeline/3,public_timeline/3]).
%-compile(export_all).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(twitterl, {sessions, delay, lastcall}).

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
    twitterl:call(Client, Method, []).
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
		       Result = "Unable to drop session.",
		       State#twitterl.sessions
    end,
    {reply, Result, State#twitterl{ sessions = NewTree }};

%%%
%%% catches  all our call calls, sets the time and tried to execute the call.
handle_call({Client, Method, Args}, _From, State) ->
    Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    Response = case session_from_client(State, Client) of
        {error, Reason} -> {error, Reason};
        {Login, Password} ->
            try apply(twitterl, Method, [Login, Password, Args])
            catch
                Err:Msg ->
                    io:format("~p:~p~n", [Err, Msg]),
                    {error, {Method, Args}}
            end;
        _ -> {error, unknown}
    end,
    {reply, Response, State#twitterl{ lastcall = Now }};

handle_call(_, _From, State) -> {noreply, ok, State}.

terminate(_Reason, State) ->
    io:format("Shutting down...~n"),
    {ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% private
session_from_client(State, Client) ->
    case gb_trees:is_defined(Client, State#twitterl.sessions) of
        false -> {error, {invalid_client, Client}};
        true -> gb_trees:get(Client, State#twitterl.sessions)
    end.

%% The following methods are used to retrieve RSS based data.
%Search based methods
find_trends(_Login, _Password, _Args) ->
    twitterl_interface:trends().
find_tweets(Login, _Password, Args) ->
    twitterl_interface:tweets(Args,Login).
find_term(_Login, _Password, Args) ->
    twitterl_interface:term(Args).

% Used to retrieve basic tweet information
tweet_timeline(_Login, _Password, _Args) ->
    twitterl_interface:public_timeline().
my_timeline(Login, _Password, _Args) ->
    twitterl_interface:user_timeline(Login).

%% These methods will return more detailed information
%% including who is friends with who & retrieving conversations.
user_followers(Login, Password, _Args) ->
    twitterl_interface:handle_user(user_followers, Login, Password,nil).
user_friends(Login, Password, _Args) ->
    twitterl_interface:handle_user(user_friends, Login, Password, nil).
user_show(Login, Password, Args) ->
    twitterl_interface:handle_user(user_show, Login, Password, Args).

public_timeline(Login, Password, _Args) ->
    twitterl_interface:handle_status(public_timeline, Login, Password, nil).
user_timeline(Login, Password, _Args) ->
    twitterl_interface:handle_status(user_timeline, Login, Password, nil).
status_show(Login, Password, Args) ->
    twitterl_interface:handle_status(status_show, Login, Password, Args).
