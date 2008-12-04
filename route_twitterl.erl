%%%-------------------------------------------------------------------
%%% File    : route_twitterl.erl
%%% Author  : Yomi (baphled boodah) Akindayini <baphled@boodah.net>
%%% Description : Twitterl router, used to route the correct tweet to
%%%               the corresponding pid.
%%%
%%% Created :  4 Dec 2008 by Yomi (baphled boodah) Akindayini <baphled@boodah.net>
%%%-------------------------------------------------------------------
-module(route_twitterl).
-import(twitterl).
%-export([start/0,stop/1]).
-compile(export_all).
start() ->
    spawn(route_twitterl, route_twitterl, []).

stop(Pid) ->
    Pid ! shutdown.

route_twitterl() ->
    receive
	{tweets_to, Pid, User} ->
	    %Data = twitterl:tweets(to, User),
	    case twitterl:tweets(to, User) of
		{ok, Results} ->
		    Pid !io:format("~p~n", [Results]);
		{error,Error} ->
		    Pid !io:format("Error: ~p~n", [Error])
	    end,
	    %Pid !io:format("Got tweets~p~n", [Data]),
	    route_twitterl();
	ok ->
	    route_twitterl();
	shutdown ->
	    io:format("Shutting down");
	Oops ->
	    io:format("Error occurred: ~p~n", [Oops])
    end.

print_results(Pid,[Result|Results]) ->
    Pid !io:format("~p~n", [Result]),
    print_results(Pid,Results);
print_results(Pid,[]) ->
    Pid !ok.
