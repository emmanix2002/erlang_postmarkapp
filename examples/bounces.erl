%%%-------------------------------------------------------------------
%%% @author eokeke
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2017 3:43 PM
%%%-------------------------------------------------------------------
-module(bounces).
-author("eokeke").
-include("../src/erlang_postmarkapp.hrl").

%% API
-export([get_bounces/0, get_bounce_tags/0]).

%% @doc we want to get a list of bounces of type "SpamComplaint"
get_bounces() ->
    ServerToken = "your server token here",
    erlang_postmarkapp:setup(ServerToken),
    case erlang_postmarkapp_bounces:get_bounces(#postmark_bounce_request{bounce_type="SpamComplaint"}) of
        [{total, TotalCount}, {bounces, Bounces}] ->
            io:format("A total of ~p bounced emails~n", [TotalCount]),
            io:format("Here's a sample: ~p~n", [lists:nth(1, Bounces)]);
        {error, Reason} -> io:format("Request failure because ~p~n", [Reason])
    end.

%% @doc we want to get a list of all tags for which we had bounces
get_bounce_tags() ->
    ServerToken = "your server token here",
    erlang_postmarkapp:setup(ServerToken),
    case erlang_postmarkapp_bounces:get_bounce_tags() of
        [ok, Tags] -> io:format("A total of ~p tags: ~p~n", [length(Tags), Tags]);
        {error, Reason} -> io:format("Request failure because ~p~n", [Reason])
    end.