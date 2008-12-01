%%%-------------------------------------------------------------------
%%% File    : twitter_api.erl
%%% Author  : Yomi (baphled boodah) Akindayini <yomi@boodah.net>
%%% Description : Basic Twitter API, used to interact with twitter.
%%%
%%% Created : 29 Nov 2008 by Yomi Akindayini <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(twitterl).

%% Will be useful for getting RSS feeds
%-import(xml_parse).

%-compile(export_all).
-export([init/0,stop/0]).
%% Search methods
-export([auth_user/2,trends/0,tweets/2,term/1]).
%% Twitter specific methods
-export([user_timeline/1,public_timeline/0,followers/2]).

-include_lib("xmerl/include/xmerl.hrl").

-define(App, "TwitterlClient/0.1").

%% Urls used to make our queries, as our api is twitter centric
%% it makes sense to define them constant initially.
-define(TwitUrl, "http://twitter.com").
-define(SearchUrl, "http://search.twitter.com").
-define(PubTimeUrl, ?TwitUrl"/statuses/public_timeline.rss").
-define(UserTimeUrl, ?TwitUrl"/statuses/user_timeline/").
-define(FollowersUrl, ?TwitUrl"/statuses/followers.rss").
-define(SearchHashUrl, ?SearchUrl"/search.rss?q=").
-define(SearchTrendsUrl, ?SearchUrl"/trends.json").
-define(VerifyUrl, ?TwitUrl"/account/verify_credentials.xml").

init() ->
    inets:start().

stop() ->
    inets:stop().

%% Retrieve the top 10 trends, only available under JSON atm.
trends() ->
    request_url(?SearchTrendsUrl).

%% Used to print out tweets to or from a specific user.
tweets(User,Type) ->
    case "from" =:= Type orelse "to" =:= Type of
	false ->
	    {error,"Wrong type"};
	true ->
	    case user_exists(User) of
		{true,_} ->
		    get_twitters(?SearchHashUrl++Type++"%3A"++User);
		{false,Error} ->
		    {error,Error}
	    end
    end.

%% Gets twitters with related to term.
%%
term(Term) ->
    get_twitters(?SearchHashUrl++"%23"++Term).

%% Used to determine whether a twitters user exists or not.
%% 
%% If an error message is found then we know that the user doesnt exist.
%%
user_exists(User) ->
    Xml = get_xml(?UserTimeUrl ++ User ++ ".rss"),
    case xmerl_xpath:string("//hash/error/text()", Xml) of
	[{xmlText, _, _, _, Error, text}] ->
	    {false, User++": "++Error};
	_ ->
	    {true, User++": found!"}
    end.

%% Retrieves a users followers.
%%
%% Needs to be worked on
%%
followers(User, Pass) ->
    case auth_user(User, Pass) of
	true ->
	    request_url(?FollowersUrl, User, Pass);
	false ->
	    {error}
    end.

%% Get a specific user's twitters.
user_timeline(User) ->
    case user_exists(User) of
	{false,Error} ->
	    {error, Error};
	_ ->
	    get_twitters(?UserTimeUrl ++ User ++ ".rss")
    end.

%% Retrieve the public timeline.
public_timeline() ->
    get_twitters(?PubTimeUrl).

%% Get the actual XML response.
get_xml(Url) ->
    {ok,{_Status,_Head,Body}} = http:request(Url),
    % Need to make sure that we don't have an error response here.
    {Xml, _Rest} = xmerl_scan:string(Body),
    Xml.

%% Get the the twitters in XML format.
get_twitters(Url) ->
    Xml = get_xml(Url),
    Twitters = xmerl_xpath:string("//item/title/text()", Xml),
    case 0 =:= length(Twitters) of
	false ->
	    print_twitters(lists:reverse(Twitters));
	_ -> {error,"Unable to find twitters."}
    end.
    

%% Loops through each of the XML twitter list and prints them out.
print_twitters([Twit|Twitters]) ->
    case Twit of
	{_,_,_,_,Title,_} ->
	    io:format("~s~n", [[Title]]),
	    print_twitters(Twitters);
	_ ->
	    {error, "Unable to read twitter"}
    end;
print_twitters([]) ->
    ok.


auth_user(Login, Password) ->
    case request_url(?VerifyUrl, Login, Password) of
        "<authorized>true</authorized>" -> true;
        _ -> false
    end.

%% Make a request to an URL.
request_url(Url) ->
    check_response(http:request(get,{Url, headers(nil, nil)}, [], [])).

%% Make an authenticated request to the specified URL.
request_url(Url, Login, Pass) ->
    check_response(http:request(get, {Url, headers(Login, Pass)}, [], [])).

%% Checks out HTTP response, if we get a 200 retrieve
%% the response body, otherwise return the status code & message.
check_response(Response) ->
    case Response of
        {ok, {Status, _Headers, Body}} -> 
	    case Status of
		{_,200,_} ->
		    Body;
		{_,Code,Msg} ->
		    {error,{Code,Msg}}
	    end;
        _ -> {error}
    end.

headers(nil, nil) -> [{"User-Agent", ?App}];
headers(User, Pass) when is_binary(User) ->
    headers(binary_to_list(User), Pass);
headers(User, Pass) when is_binary(Pass) ->
    headers(User, binary_to_list(Pass));
headers(User, Pass) -> 
    Auth = base64:encode(User ++ ":" ++ Pass),
    Basic = lists:flatten(io_lib:fwrite("Basic ~s", [Auth])),
    [{"User-Agent", ?App}, {"Authorization", Basic}].
