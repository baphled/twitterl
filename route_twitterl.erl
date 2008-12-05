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

%-export([start/0,stop/0,get_trends/1]).
-compile(export_all).

start() ->
    spawn(route_twitterl, route_twitterl, []).

stop(Pid) ->
    Pid ! shutdown.

get_trends(RoutePid) ->
  {trends, RoutePid}.
   %RoutePid ! print_results(Result).

tweets_to(User, RoutePid) ->
    {tweets, RoutePid,{to, User}}.

tweets_from(User, RoutePid) ->
    {tweets, RoutePid, {from, User}}.

get_term(Term, RoutePid) ->
    {term, RoutePid, Term}.

public_timeline(RoutePid) ->
    {public_timeline, RoutePid}.

route_twitterl() ->
    receive
	{trends, Pid} ->
	    case twitterl:trends() of
		{ok, Result} ->
		    Data = tuple_to_list(Result),
		    Pid !print_results(Data);
		{_,Error}  ->
		    Pid !io:format("Error: ~p~n", [Error]),
		    route_twitterl()
	    end;
	    %route_twitterl;
	{tweets, Pid, {Type,User}} ->
	    case twitterl:tweets(Type, User) of
		{ok, Results} ->
		    Pid !print_results(Results);
		{error,Error} ->
		    Pid !io:format("Error: ~p~n", [Error]),
		    route_twitterl()
	    end;
	    %route_twitterl();
	{term, Pid, Term} ->
	    case twitterl:term(Term) of
		{ok, Results} ->
		    Pid !print_results(Results);
		{error,Error} ->
		    Pid !io:format("Error: ~p~n", [Error]),
		    route_twitterl()
	    end;
	    %route_twitterl();
	{public_timeline, Pid} ->
	    case twitterl:public_timeline() of
		{ok, Results} ->
		    Pid !print_results(Results);
		{error, Error} ->
		    Pid !io:format("Error: ~p~n", [Error]),
		    route_twitterl()
	    end;
	    %route_twitterl();
	shutdown ->
	    io:format("Shutting down~n");
	Oops ->
	    io:format("Error occurred: ~p~n", [Oops]),
	    route_twitterl()
    end.

print_results([Result|Results]) ->
    io:format("~p~n", [Result]),
    print_results(Results);
print_results([]) ->
    route_twitterl().
