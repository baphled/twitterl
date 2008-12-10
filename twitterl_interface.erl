%%%-------------------------------------------------------------------
%%% File    : twitterl.erl
%%% Author  : Yomi (baphled boodah) Akindayini <yomi@boodah.net>
%%% Description : Basic Twitter ineterface, used by twitterl to retrieve
%%% search and status information from twitter.
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

-record(status, {created_at, id, text, source, truncated, in_reply_to_status_id, in_reply_to_user_id, favorited,user}).
-record(tweet, {title, pubDate, link}).
-record(user, {id, name, screen_name, location, description, profile_image_url, url, protected, followers_count, friends_count, created_at, favourites_count, utc_offset, time_zone, following, notifications, statuses_count,status}).

-export([handle_status/3,handle_status/4,handle_user/3,handle_user/4]).

% Search methods
-export([auth_user/2,trends/0,tweets/2,term/1]).
%% Twitter specific methods
-export([user_timeline/1,public_timeline/0]).

-include_lib("xmerl/include/xmerl.hrl").

-define(App, "TwitterlClient/0.1").

%% Urls used to make our queries, as our api is twitter centric
%% it makes sense to define them constant initially.
-define(TwitUrl, "http://twitter.com").
-define(SearchUrl, "http://search.twitter.com").
-define(PubTimeUrl, ?TwitUrl"/statuses/public_timeline.rss").
-define(UserTimeUrl, ?TwitUrl"/statuses/user_timeline/").
-define(UsersUrl, ?TwitUrl"/users/").
-define(StatusesUrl, ?TwitUrl"/statuses/").
-define(SearchHashUrl, ?SearchUrl"/search.rss?q=").
-define(SearchTrendsUrl, ?SearchUrl"/trends.json").
-define(VerifyUrl, ?TwitUrl"/account/verify_credentials.xml").
			 
%% Retrieve the top 10 trends, only available under JSON atm.
%% Seems to be a bug in the parsing, sometimes we get a mismatch
%% causing an error.
trends() ->
    parse_json(?SearchTrendsUrl).

parse_json(Url) ->
    case request_url(Url, nil, nil) of
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
    case get_xml(?UserTimeUrl ++ User ++ ".rss") of
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

%% Methods to retrieve user based information.

%%% Handles each of our user & status requests
%@private
handle_status(Type,User,Pass) ->
    handle_status(Type,User,Pass,nil).
handle_status(Type,User,Pass,Args) ->
    case Type of
	user_timeline ->
	    get_status(?StatusesUrl++"user_timeline.xml",User,Pass);
	public_timeline ->
	    get_status(?StatusesUrl++"public_timeline.xml",User,Pass);
	status_show ->
	    case is_list(Args) of
		true -> get_status(?StatusesUrl++"show.xml?id="++Args, User,Pass);
		_ -> {error, {Type, Args}}
	    end;
	_ ->
	    {error,"Invalid status type"}
    end.

handle_user(Type,User,Pass) ->
    handle_user(Type,User,Pass,nil).
handle_user(Type,User,Pass,Args) ->
    case Type of
	user_followers ->
	    get_user(?StatusesUrl++"followers.xml",User,Pass);
	user_friends ->
            get_user(?StatusesUrl++"friends.xml",User,Pass);
	user_show ->
	    case is_list(Args) of
		true -> get_user(?UsersUrl"show/" ++ Args ++ ".xml", User,Pass);
		_ -> {error, {Type, Args}}
	    end
   end.
get_user(Url,User,Pass) ->
    case get_xml(Url, User, Pass) of
	{ok,Xml} ->
	    lists:reverse(parse_users(Xml));
	{error,Error} ->
	    {error,Error}
    end.

get_status(Url,User,Pass) ->
     case get_xml(Url, User, Pass) of
	{ok,Xml} ->
	    lists:reverse(parse_statuses(Xml));
	{error,Error} ->
	    {error,Error}
    end.

%% Parsing functionality
get_xml(Url) ->
    get_xml(Url, nil, nil).
%% Get the actual XML response.
get_xml(Url,Login,Password) ->
    case request_url(Url,Login,Password) of
	{ok, Body} ->
	    {ok, get_body(Body)}; 
	{error, Error} ->
	    {error, Error}
    end.

%% Get the the twitters in XML format.
get_twitters(Url) ->
    case get_xml(Url,nil, nil) of
	{ok,Xml} ->
	    lists:reverse(parse_items(Xml));
	{error,Error} ->
	    {error,Error}
    end.

get_body(Html) ->
    case xmerl_scan:string(Html) of
	{Xml, _Rest} ->
	     Xml;
	_ ->
	    {error,"Unable to parse XML"}
    end.

%% Parses our XML
parse_items(Xml) ->
    [parse_item(Item) || Item <- xmerl_xpath:string("/rss/channel/item", Xml)].

parse_users(Xml) ->
    [parse_user(User) || User <- xmerl_xpath:string("//users/user|//user",Xml)].

parse_statuses(Xml) ->
    [parse_status(Status) || Status <- xmerl_xpath:string("//statuses/status|//status",Xml)].

parse_status(Xml) when is_list(Xml) ->
    xmerl_xpath:string("/status",Xml);
parse_status(Node) when is_tuple(Node)->
    Status = #status{
      created_at = format_text(Node, ["/status/created_at/text()"],""),
      id = format_text(Node, ["/status/id/text()"],""),
      text = format_text(Node, ["/status/text/text()"],""),
      source = format_text(Node, ["/status/source/text()"],""),
      truncated = format_text(Node, ["/status/truncated/text()"],""),
      in_reply_to_status_id = format_text(Node, ["/status/in_reply_to_status_id/text()"],""),
      in_reply_to_user_id = format_text(Node, ["/status/in_reply_to_user_id/text()"],""),
      favorited = format_text(Node, ["/status/favourited/text()"],"")
    },
    case xmerl_xpath:string("/status/user", Node) of
        [] ->  Status;
        [User] -> Status#status{ user = parse_user(User) }
    end.

parse_item(Node) ->
    Item = #tweet {
      title = format_text(Node, ["/item/title/text()"],""),
      pubDate = format_text(Node, ["/item/pubDate/text()"],""),
      link = format_text(Node, ["/item/link/text()"], "")
    },
    Item.

%%% Parses our user information.
%%% Should really find a better way of doing this, we repeating ourselves
%%%
parse_user(Node) ->
    User = #user {
      id = format_text(Node,["/user/id/text()"], ""),
      name = format_text(Node, ["/user/name/text()"], ""),
      screen_name = format_text(Node, ["/user/screen_name/text()"], ""),
      location = format_text(Node, ["/user/location/text()"], ""),
      description = format_text(Node, ["/user/description/text()"], ""),
      profile_image_url = format_text(Node, ["/user/profile_image_url/text()"], ""),
      url = format_text(Node, ["/user/url/text()"], ""),
      protected = format_text(Node, ["/user/protected/text()"], ""),
      followers_count = format_text(Node, ["/user/followers_count/text()"], ""),
      friends_count = format_text(Node, ["/user/friends_count/text()"], ""),
      created_at = format_text(Node, ["/user/created_at/text()"], ""),
      favourites_count = format_text(Node, ["/user/favourites_count/text()"], ""),
      utc_offset = format_text(Node, ["/user/utc_offset/text()"], ""),
      time_zone = format_text(Node, ["/user/time_zone/text()"], ""),
      following = format_text(Node, ["/user/following/text()"], ""),
      notifications = format_text(Node, ["/user/notifications/text()"], ""),
      statuses_count = format_text(Node, ["/user/statuses_count/text()"], "")
    },
    case xmerl_xpath:string("/user/status", Node) of
        [] -> User;
        [Status] -> User#user{ status = parse_status(Status) }
    end.

%% @private
format_text(_, [], Result) -> Result;
format_text(Xml, [Xpath | Tail], Result) ->
    Results = lists:foldr(
        fun(#xmlText{value = Value}, Acc) ->
	   lists:append(Value, Acc);
           (_, Acc) -> Acc
        end,
        Result,
        xmerl_xpath:string(Xpath, Xml)
    ),
    format_text(Xml, Tail, Results).

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

