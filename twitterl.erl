%%%-------------------------------------------------------------------
%%% File    : twitterl.erl
%%% Author  : Yomi (baphled boodah) Akindayini <yomi@boodah.net>
%%% Description : Basic Twitter API, used to interact with twitter.
%%%
%%% twitterl is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% twitterl is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with json_parser.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Created : 29 Nov 2008 by Yomi Akindayini <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(twitterl).
-author("Yomi (baphled) Akindayini").

%% Will be useful for getting RSS feedsparsing JSON
-import(json_parser).

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
    case request_url(?SearchTrendsUrl) of
	{ok,Body} ->
	    Json = json_parser:dvm_parser(list_to_binary(Body)),
	    Json;
	{error,Error} ->
	    {error,Error}
    end.

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
    {ok,Xml} = get_xml(?UserTimeUrl ++ User ++ ".rss"),
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
	    case request_url(?FollowersUrl, User, Pass) of
		{ok,Body} ->
		    Body;
		{error,Error} ->
		    {error,Error}
	    end;
	false ->
	    {error}
    end.

%% Get a specific user's twitters.
%% Don't really need any more, seeing as tweets will do the same thing.
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
    case request_url(Url) of
	{ok,Body} ->
	    {Xml, _Rest} = xmerl_scan:string(Body),
	    {ok,Xml};
	{error,Error} ->
	    {error,Error}
    end.

%% Get the the twitters in XML format.
get_twitters(Url) ->
    case get_xml(Url) of
	{ok,Xml} ->
	    parse_xml(Xml);
	{error,Error} ->
	    {error,Error}
    end.

%% Parses our XML sending each tweet to parse_twitters
parse_xml(Xml) ->
    Twitters = xmerl_xpath:string("//item/title/text()", Xml),
    case 0 =:= length(Twitters) of
	false ->
	    parse_twitters(Twitters);
	_ ->
	    {error,"Unable to find twitters."}
    end.
    

%% Loops through each of the XML twitter list and prints them out.
parse_twitters([Tweet|Twitters]) ->
    case Tweet of
	{_,_,_,_,Title,_} ->
	    io:format("~s~n", [[Title]]),
	    parse_twitters(lists:reverse(Twitters));
	_ ->
	    {error, "Unable to read twitter"}
    end;
parse_twitters([]) ->
    ok.

%% Checks to see if the user can actually log in.
auth_user(Login, Password) ->
    case request_url(?VerifyUrl, Login, Password) of
        {ok,Body} ->
	    case Body of
		    "<authorized>true</authorized>" -> true;
		    _ -> false
	    end;
	 {error,Error} ->
	    {error,Error}
    end.

%% Make a request to an URL.
request_url(Url) ->
    check_response(http:request(get, {Url, headers(nil, nil)}, [], [])).
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
		    {ok,Body};
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
