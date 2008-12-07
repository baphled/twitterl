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
-module(twitterl_interface).
-author("Yomi (baphled) Akindayini").
-vsn(01).
-purpose("A basic Twitter client.").

%% Will be useful for getting RSS feedsparsing JSON
-import(json_parser).

-record(tweet, {guid, title, description, date, link}).
-record(user, {id, name, screen_name, location, description, profile_image_url, url, protected, followers_count}).
-record(status, {created_at,id,text,source, truncated,in_reply_to_status_id, in_reply_to_user_id, favorited}).

% Search methods
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
-define(FollowersUrl, ?TwitUrl"/statuses/followers.xml").
-define(SearchHashUrl, ?SearchUrl"/search.rss?q=").
-define(SearchTrendsUrl, ?SearchUrl"/trends.json").
-define(VerifyUrl, ?TwitUrl"/account/verify_credentials.xml").
			 
%% Retrieve the top 10 trends, only available under JSON atm.
%% Seems to be a bug in the parsing, sometimes we get a mismatch
%% causing an error.
trends() ->
    case request_url(?SearchTrendsUrl, nil, nil) of
	{ok,Body} ->
	    Json = json_parser:dvm_parser(list_to_binary(Body)),
	    {ok,{struct,Reply},_} = Json,
	    [H|_T] = Reply,
	    case H of
		{_,Result} ->
		    case is_binary(Result) of
			false ->
			    {ok,loop_json([],Result)};
		        true ->
			    trends()
		    end;
		_ ->
		    {error,'Can not retrieve trends.'}
	    end;
	{error,Error} ->
	    {error,Error}
    end.

%% Loop through our JSON result retrieving each result & turning
%% into the desired format.
loop_json(List,[H|T]) ->
    {_,Json} = H,
    [{_,Title},{_,Value}] = Json,
    Data = [[binary_to_list(Title),binary_to_list(Value)]|List],
    loop_json(Data,T);
loop_json(List,[]) ->
    list_to_tuple(List).

%% Used to print out tweets to or from a specific user.
tweets(Type,User) ->
    case from =:= Type orelse to =:= Type andalso is_atom(Type) of
	false ->
	    {error,"Wrong type"};
	true ->
	    case user_exists(User) of
		{true,_} ->
		    get_twitters(?SearchHashUrl++atom_to_list(Type)++"%3A"++User);
		{false,Error} ->
		    {false,Error};
		{error, Error} ->
		    {error, Error}
	    end
    end.

%% Gets twitters with related to term.
%%
term(Term) -> 
    case is_list(Term) of
	true -> get_twitters(?SearchHashUrl++"%23"++Term);
	_ -> {error, 'Term must be a list'}
    end.

%% Used to determine whether a twitters user exists or not.
%% 
%% If an error message is found then we know that the user doesnt exist.
%%
user_exists(User) ->
    case get_xml(?UserTimeUrl ++ User ++ ".rss", nil, nil) of
	{error,Error} ->
	    {error, Error};
	{ok, Xml} ->
	    case xmerl_xpath:string("//hash/error/text()", Xml) of
		[{xmlText, _, _, _, Error, text}] ->
		    {false, User++": "++Error};
		_ ->
		    {true, User++": found!"}
	    end
    end.

%% Retrieves a users followers.
%%
%% Needs to be worked on
%%
followers(User, Pass) ->
    case get_xml(?FollowersUrl, User, Pass) of
	{ok,Xml} ->
	    parse_users(Xml);
	{error,Error} ->
	    {error,Error}
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
get_xml(Url,nil,nil) ->
    get_xml(Url,nil,nil);
%% Get the actual XML response.
get_xml(Url,Login,Password) ->
    case request_url(Url,Login,Password) of
	    {ok, Body} ->
	       {Xml, _Rest} = xmerl_scan:string(Body),
	       {ok, Xml};
	    {error, Error} ->
	       {error, Error}
    end.
%% Get the the twitters in XML format.
get_twitters(Url) ->
    case get_xml(Url,nil, nil) of
	{ok,Xml} ->
	    parse_xml(Xml,"//item/title/text()");
	{error,Error} ->
	    {error,Error}
    end.

parse_users(Xml) ->
    [parse_user(User) || User <- xmerl_xpath:string("/users/user",Xml)].

parse_user(Xml) ->
    User = #user {
      id = format_text(Xml,["/user/id/text()"], ""),
      name = format_text(Xml, ["/user/name/text()"], ""),
      screen_name = format_text(Xml, ["/user/screen_name/text()"], "")
      }.

%% Parses our XML sending each tweet to parse_twitters
parse_xml(Xml,XPath) ->
    Twitters = xmerl_xpath:string(XPath, Xml),
    case 0 =:= length(Twitters) of
	false ->
	    parse_twitters([],Twitters);
	_ ->
	    {error,"Unable to find twitters."}
    end.

%% Loops through each of the XML twitter list and prints them out.
parse_twitters(Tweets,[Tweet|Twitters]) ->
    case Tweet of
	{_,_,_,_,Title,_} ->
	    MergedList = [Title| Tweets],
	    parse_twitters(MergedList,Twitters);
	_ ->
	    {error, "Unable to read twitter"}
    end;
parse_twitters(List,[]) ->
    {ok,List}.

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
request_url(Url,nil,nil) ->
    check_response(http:request(get, {Url, headers(nil, nil)}, [], []));
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

%% @private
format_text(_, [], Result) -> Result;
format_text(Xml, [Xpath | Tail], Result) ->
    Results = lists:foldr(
        fun(#xmlText{value = Value}, Acc) -> lists:append(Value, Acc);
           (_, Acc) -> Acc
        end,
        Result,
        xmerl_xpath:string(Xpath, Xml)
    ),
    format_text(Xml, Tail, Results).
