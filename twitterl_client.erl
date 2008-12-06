%%%-------------------------------------------------------------------
%%% File    : twitterl_client.erl
%%% Author  : Yomi (baphled boodah) Akindayini <baphled@boodah.net>
%%% Description : Twitterl router, used to route the correct tweet to
%%%               the corresponding pid.
%%%
%%% Created :  4 Dec 2008 by Yomi (baphled boodah) Akindayini <baphled@boodah.net>
%%%-------------------------------------------------------------------
-module(twitterl_client).
-import(twitterl).
-define(SERVER, twitterl_client).

-compile(export_all).

start() ->
    global:trans({?SERVER, ?SERVER},
		 fun() ->
			 case global:whereis_name(?SERVER) of
			     undefined ->
				 Pid = spawn(twitterl_client, handle_twitterl, []),
				 global:register_name(?SERVER, Pid);
			     _ ->
				 ok
			 end
		 end).

stop() ->
    global:trans({?SERVER, ?SERVER},
		 fun() ->
			 case global:whereis_name(?SERVER) of
			     undefined ->
				 ok;
			     _ ->
				 global:send(?SERVER, shutdown)
			 end
		 end).

get_trends() ->
    %global:send(?SERVER,{trends}).
    {trends}.
    

tweets_to(User) ->
    global:send(?SERVER, {tweets,{to, User}}).

tweets_from(User) ->
    global:send(?SERVER, {tweets, {from, User}}).

get_term(Term) ->
    global:send(?SERVER, {term, Term}).

public_timeline() ->
    global:send(?SERVER, {public_timeline}).

handle_twitterl() ->
    receive
	{trends} ->
	    case twitterl:trends() of
		{ok, Result} ->
		    Data = tuple_to_list(Result),
		    print_results(Data);
		{_,Error}  ->
		    io:format("Error: ~p~n", [Error]),
		    handle_twitterl()
	    end;
	{tweets, {Type,User}} ->
	    case twitterl:tweets(Type, User) of
		{ok, Results} ->
		    print_results(Results);
		{error,Error} ->
		    io:format("Error: ~p~n", [Error]),
		    handle_twitterl()
	    end;
	{term, Term} ->
	    case twitterl:term(Term) of
		{ok, Results} ->
		    print_results(Results);
		{error,Error} ->
		    io:format("Error: ~p~n", [Error]),
		    handle_twitterl()
	    end;
	{public_timeline} ->
	    case twitterl:public_timeline() of
		{ok, Results} ->
		    print_results(Results);
		{error, Error} ->
		    io:format("Error: ~p~n", [Error]),
		    handle_twitterl()
	    end;
	shutdown ->
	    io:format("Shutting down~n");
	Oops ->
	    io:format("Error occurred: ~p~n", [Oops]),
	    handle_twitterl()
    end.

print_results([Result|Results]) ->
    io:format("~p~n", [Result]),
    print_results(Results);
print_results([]) ->
    handle_twitterl().
