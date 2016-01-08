%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for mochix.

-module(mochix_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                  % Takes all requests to /findtweets and sends them to
                  % a function.
                  % .../findtweets?query=jobs&loc=57.726120,11.942240,10km
                  "findtweets"  -> spawn(fun () -> findget(Req) end);
                  % Get statistics
                  % /stats?query=jobs
                  "stats"       -> spawn(fun () -> get_stats(Req) end);
                  _ ->
                    Req:respond({200, [{"Content-Type", "text/plain"}],
                    "You are doing it wrong!\n"})
                end;
            'POST' ->
                case Path of
                    _ ->
                      Req:respond({200, [{"Content-Type", "text/plain"}],
                      "You are doing it wrong!\n"})
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

% Takes a query from an HTTP request and gives back an answer encoded as JSON
findget(Req) ->
  % Parse data from the URL
  QueryStringData = Req:parse_qs(),
  % Find the parameters used for the request
  Query           = proplists:get_value("query", QueryStringData),
  io:format("findget Query: ~p~n", [Query]),
  Location        = proplists:get_value("loc", QueryStringData),
  % Send parameters to another Erlang function that returns twitter data
  Search          = projectx_app:get_tweets(Query, Location),
  % Format the JSON data
  JSONStruct      = {struct, Search},
  % Encode as JSON Values to display on a webpage
  HTMLoutput      = mochijson2:encode(JSONStruct),
  Req:respond({200, [{"Content-Type", "text/plain"}],
                HTMLoutput}).

% Takes a query from an HTTP request and gives back an answer encoded as JSON
get_stats(Req) ->
  % Parse data from the URL
  QueryStringData = Req:parse_qs(),
  % Find the parameters used for the request
  Query           = proplists:get_value("query", QueryStringData),
  BinaryQuery     = list_to_binary([Query]),
  io:format("getstats BinaryQuery: ~p~n", [BinaryQuery]),
  % Send parameters to another Erlang function that returns data from our DB
  Stats           = resmanager_server:get_res(BinaryQuery),
  % Encode as JSON Values to display on a webpage
  HTMLoutput      = mochijson2:encode(Stats),
  Req:respond({200, [{"Content-Type", "text/plain"}],
              HTMLoutput}).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
