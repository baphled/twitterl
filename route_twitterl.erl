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
-define(SERVER, route_twitterl).

-compile(export_all).

start() ->
    Pid = spawn(route_twitterl, route_twitterl, []),
    erlang:register(?SERVER, Pid),
    Pid.

stop() ->
    ?SERVER ! shutdown.

get_trends() ->
  ?SERVER !{trends}.

tweets_to(User) ->
    ?SERVER !{tweets,{to, User}}.

tweets_from(User) ->
    ?SERVER !{tweets, {from, User}}.

get_term(Term) ->
    ?SERVER !{term, Term}.

public_timeline() ->
    ?SERVER !{public_timeline}.

route_twitterl() ->
    receive
	{trends} ->
	    case twitterl:trends() of
		{ok, Result} ->
		    Data = tuple_to_list(Result),
		    print_results(Data);
		{_,Error}  ->
		    io:format("Error: ~p~n", [Error]),
		    route_twitterl()
	    end;
	{tweets, {Type,User}} ->
	    case twitterl:tweets(Type, User) of
		{ok, Results} ->
		    print_results(Results);
		{error,Error} ->
		    io:format("Error: ~p~n", [Error]),
		    route_twitterl()
	    end;
	{term, Term} ->
	    case twitterl:term(Term) of
		{ok, Results} ->
		    print_results(Results);
		{error,Error} ->
		    io:format("Error: ~p~n", [Error]),
		    route_twitterl()
	    end;
	{public_timeline} ->
	    case twitterl:public_timeline() of
		{ok, Results} ->
		    print_results(Results);
		{error, Error} ->
		    io:format("Error: ~p~n", [Error]),
		    route_twitterl()
	    end;
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
