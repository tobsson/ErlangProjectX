-module(projectx_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, init/0, get_tweets/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    projectx_sup:start_link().

stop(_State) ->
    ok.
% Start the application
% erl -pa deps/*/ebin -pa ebin -s crypto -s ssl
% projectx_app:init()
init() ->
  ssl:start(),
  %register(tweet, spawn_link(fun loop/0)),
  io:format("Initialized projectx_app: ~n").

% This function will take the value of 'Query' and perform a search on
% twitter, returns JSONobjects as they come from Twitter servers
get_tweets(Query, Location) ->
  % Start ibrowse which we use for our network connections.
  ibrowse:start(),
  % Get Bearer token from twitter which we need for Application Auth
  BearerToken = app_auth(),
  %io:format("get_ tweets: BearerToken: ~p~n", [BearerToken]),
  % Construct the HTTP request with authentication and search parameters
  HeaderAuth = [{"Authorization","Bearer " ++ BearerToken}],
  URL = url_creator(Query, Location),

  % Request sent to Twitter
  {ok,_,_,TweetData} = ibrowse:send_req(URL, HeaderAuth, get),
  %io:format("get_tweets Returns: ~p~n", [TweetData]),

  Jiffied = jiffy_decode(TweetData),
  io:format("Jiffied done ~n"),
  AllText = extract_only_text(Jiffied, []),
  io:format("AllText done ~n"),
  Sentiment = word_server:textlist_val([<<"trying to do this with a shorter text">>,<<"trying to do this with a shorter text">>]),
  io:format("sentiment set: ~p~n", [Sentiment]),
  Rando = random_tweets(Jiffied, [], 10),
  io:format("rando value set: ~p~n", [Rando]),
  Sentiment ++ Rando.

% This function
url_creator(Query, undefined) ->
  URIQuery  = http_uri:encode(Query),
  string:concat(string:concat(
          "https://api.twitter.com/1.1/search/tweets.json?q=",URIQuery),
            "&count=15&lang=en&result_type=recent");
url_creator(Query, Location) ->
  URIQuery  = http_uri:encode(Query),
  io:format("url_creator Location: ~p~n", [Location]),
  string:concat(string:concat(string:concat(
          "https://api.twitter.com/1.1/search/tweets.json?q=",URIQuery),
            "&count=15&lang=en&result_type=recent&geocode="), Location).

% Takes a JSON object and makes it more readable.
jiffy_decode(A) ->
  TweetDataDecoded = jiffy:decode(A),
  {TDD} = TweetDataDecoded, % extracts list from first tuple
  {_Key, Value} = lists:keyfind(<<"statuses">>, 1, TDD), % extracts the only tuple "statuses" from list
  %io:format("jiffy_decode: ~p~n", [Value]),
  Value.

% Chooses 3 random tweets
	random_tweets(Value, Data, 7) -> Data;
	random_tweets(Value, Data, N) ->
		{RandomTweet} = lists:nth(random:uniform(N), Value),% random:uniform(N) chooses random number from range 1-N
		Value1 = lists:delete({RandomTweet}, Value),
		{KeyUser, ValueUser} = lists:keyfind(<<"user">>, 1, RandomTweet),
		{VUser} = ValueUser,
		{KeyName, UserName} = lists:keyfind(<<"name">>, 1, VUser),
		{RKey, RandomText} = lists:keyfind(<<"text">>, 1, RandomTweet),
		random_tweets(Value, Data ++ [UserName] ++ [RandomText], N-1).
%Loops it (loop should be executed only 3 times) and put usernames and tweets in a loop


% Extracts only the fields with "text" from JSON.
% Data is a Binary List
extract_only_text([], Data) -> Data;
extract_only_text(List, Data) ->
  % Extracts first tuple from the list Value
  {Head} = hd(List),
  {_Key, Text} = lists:keyfind(<<"text">>, 1, Head),
  %io:format("Text: ~p~n", [Text]),
  extract_only_text(tl(List), Data ++ [Text]).

sentiment(Data) ->
  io:format("Sentiment started. ~p~n", [Data]),
  Pids = [spawn(fun() -> collect_sentiment(self(), X) end)
             || X <- Data],
  io:format("Sentiment started PIDS: ~p~n", [Pids]),
  Result = [rec_sentiment(P) || P <- Pids].
  %collect_sentiment(length(Pids), []).
  %io:format("sentiment set ~n"),
  %P ! {sentiment_sent, Sentiment}.

%collect_sentiment(0, List) -> io:format("sentiment finished.~n"),List;
collect_sentiment(P, Data) ->
  %io:format("collect_sentiment started. ~p~n", []),
  Sentiment = word_server:text_val(Data),
  io:format("Sentiment value: ~p~n", [Sentiment]),
  P ! {sentiment_sent, self(), Sentiment}.

rec_sentiment(P) ->
  io:format("rec_sentiment started.~n"),
  receive
    {sentiment_sent, P, X} -> X
  end.


% This function returns a Bearer Token from Twitter
% that's needed for Application Authentication
app_auth() ->
  % ConsumerKey and Secret are specific to this an app and can be found here:
  % https://apps.twitter.com/
  % This function will encode the values, construct the HTTP request and
  % return a Bearer Token as a list
  ConsumerKey     = "JNEVsG01mMdYIKq404MMmXw2H",
  ConsumerSecret  = "aeNjhFV0F02PjNktQEBN2ESGbbzbDlcNeVufBxsKI9B0qFjFhB",
  EncodedConsumerKey    = http_uri:encode(ConsumerKey),
  EncodedConsumerSecret = http_uri:encode(ConsumerSecret),
  ReadyToBase = string:concat(
    string:concat(EncodedConsumerKey, ":"), EncodedConsumerSecret),
  Based   = base64:encode_to_string(ReadyToBase),
  Body    = "grant_type=client_credentials",
  Headers = [{"Authorization","Basic " ++ Based},
              {"Content-Type","application/x-www-form-urlencoded;charset=UTF-8"}],
  % Request the Bearer Token
  {ok,_,_,TokenAnswer} = ibrowse:send_req(
                            "https://api.twitter.com/oauth2/token",
                                Headers, post, Body),

  % Turn the answer into something more readable
  {TokenDecoded}    = jiffy:decode(TokenAnswer),
  %io:format("app_auth: BearerAnswer: ~p~n", [TokenDecoded]),
  % Check that it really is a bearer token
  %{_,TokenType}     = lists:keyfind(<<"token_type">>, 1, TokenDecoded),
  % Extract the Token and put it into a list.
  {_,BearerToken}   = lists:keyfind(<<"access_token">>, 1, TokenDecoded),
  BearerTokenString = binary:bin_to_list(BearerToken),
  BearerTokenString.
