%%%-------------------------------------------------------------------
%%% @author eokeke
%%% @copyright (C) 2017
%%% @doc
%%% This module contains functions for dealing with bounces.
%%% @end
%%% @reference http://developer.postmarkapp.com/developer-api-bounce.html
%%% Created : 20. Jan 2017 10:14 AM
%%%-------------------------------------------------------------------
-module(erlang_postmarkapp_bounces).
-author("eokeke").
-include("erlang_postmarkapp.hrl").

%% API
-export([
    activate_bounce/1,
    get_delivery_stats/0,
    get_bounces/1,
    get_bounce/1,
    get_bounce_dump/1,
    get_bounce_tags/0
]).

-type deliveryStat() :: {inactive_mails, integer()} | {bounces, [#postmark_bounce_stats{}]}.
-type deliveryStatsResponse() :: [deliveryStat()] | {error, string()}.
-type bounce() :: #postmark_bounce{}.
-type bounces() :: [bounce()].
-type bouncesInfo() :: {total, integer()} | {bounces, bounces()}.
-type bouncesResponse() :: [bouncesInfo()].
-type tag() :: string().
-type tags() :: [tag()] | [].

%%====================================================================
%% API functions
%%====================================================================

%% @doc Get an overview of the delivery statistics for all email that has been sent through this Server.
-spec get_delivery_stats() -> deliveryStatsResponse().
get_delivery_stats() ->
    RequestHeaders = erlang_postmarkapp_request:get_default_headers(),
    case erlang_postmarkapp_request:request(get, ?POSTMARK_ENDPOINT_DELIVERY_STATS, "", RequestHeaders) of
        [{headers, _}, {body, Json}] ->
            case process_delivery_stats_response(Json) of
                [{inactive_mails, InactiveMails}, {bounces, BounceStatRecords}] ->
                    [{inactive_mails, InactiveMails}, {bounces, BounceStatRecords}];
                Error -> Error
            end;
        {error, fail, Reason} -> {error, Reason};
        {error, StatusCode, _Body} ->
            Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
            {error, Message}
    end.

%% @doc Get a batch of bounces to be processed.
%% It queries the Postmark API for bounces based on the values passed in the <code>#postmark_bounce_request{}</code> record
-spec get_bounces(BounceRequestRecord::#postmark_bounce_request{}) -> bouncesResponse() | {error, string()}.
get_bounces(BounceRequestRecord) when is_record(BounceRequestRecord, postmark_bounce_request) ->
    Data = bounce_request_to_list(BounceRequestRecord),
    QueryString = erlang_postmarkapp_request:http_build_query(Data),
    RequestUrl = string:join([?POSTMARK_ENDPOINT_BOUNCES, "?", QueryString], ""),
    RequestHeaders = erlang_postmarkapp_request:get_default_headers(),
    case erlang_postmarkapp_request:request(get, RequestUrl, "", RequestHeaders) of
        [{headers, _}, {body, Json}] ->
            case process_bounces_response(Json) of
                [{total, Total}, {bounces, BounceRecordList}] ->
                    [{total, Total}, {bounces, BounceRecordList}];
                Error -> Error
            end;
        {error, fail, Reason} -> {error, Reason};
        {error, StatusCode, _Body} ->
            Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
            {error, Message}
    end;

%% @doc queries the Postmark API for bounces based on the values passed in the `#postmark_bounce_request{}` record
get_bounces(_) ->
    {error, "You need to pass an instance of the postmark_bounce_request record"}.

%% @doc Locate information on a specific email bounce.
-spec get_bounce(BounceId::integer()) -> {ok, bounce()} | {error, string()}.
get_bounce(BounceId) when is_integer(BounceId) ->
    RequestUrl = string:join([?POSTMARK_ENDPOINT_BOUNCES, integer_to_list(BounceId)], "/"),
    RequestHeaders = erlang_postmarkapp_request:get_default_headers(),
    case erlang_postmarkapp_request:request(get, RequestUrl, "", RequestHeaders) of
        [{headers, _}, {body, Json}] ->
            case process_bounce_response(Json) of
                {postmark_bounce, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Bounce ->
                    {ok, Bounce};
                [] -> {error, "No data was found in the response"}
            end;
        {error, fail, Reason} -> {error, Reason};
        {error, StatusCode, _Body} ->
            Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
            {error, Message}
    end;

%% @doc gets the details of a particular bounce, based on the supplied id.
get_bounce(_) ->
    {error, "You need to pass an integer id of the bounce"}.

%% @doc Get a "dump" for a specific bounce.
-spec get_bounce_dump(BounceId::integer()) -> {ok, string()} | {error, string()}.
get_bounce_dump(BounceId) when is_integer(BounceId) ->
    RequestUrl = string:join([
        ?POSTMARK_ENDPOINT_BOUNCES,
        integer_to_list(BounceId),
        ?POSTMARK_ENDPOINT_DUMP
    ], "/"),
    RequestHeaders = erlang_postmarkapp_request:get_default_headers(),
    case erlang_postmarkapp_request:request(get, RequestUrl, "", RequestHeaders) of
        [{headers, _}, {body, Json}] ->
            case erlang_postmarkapp:get_response_data("Body", Json) of
                undefined -> {error, "Could not properly process the API response. No 'Body' attribute found"};
                Body -> {ok, Body}
            end;
        {error, fail, Reason} -> {error, Reason};
        {error, StatusCode, _Body} ->
            Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
            {error, Message}
    end;

%% @doc returns the dump for the bounce
get_bounce_dump(_) ->
    {error, "You need to pass an integer id of the bounce"}.

%% @doc Cause the email address associated with a Bounce to be reactivated.
-spec activate_bounce(BounceId::integer()) -> {ok, bounce()} | {error, string()}.
activate_bounce(BounceId) when is_integer(BounceId) ->
    RequestUrl = string:join([
        ?POSTMARK_ENDPOINT_BOUNCES,
        integer_to_list(BounceId),
        ?POSTMARK_ENDPOINT_ACTIVATE
    ], "/"),
    case erlang_postmarkapp_request:request(put, RequestUrl, {string, "{}"}) of
        [{headers, _}, {body, Json}] ->
            case process_activate_response(Json) of
                {ok, Bounce} -> {ok, Bounce};
                {error, Message} -> {error, Message}
            end;
        {error, fail, Reason} -> {error, Reason};
        {error, StatusCode, _Body} ->
            Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
            {error, Message}
    end;

%% @doc activates a bounce
activate_bounce(_) ->
    {error, "You need to pass an integer id of the bounce"}.

%% @doc Get the list of tags associated with messages that have bounced.
-spec get_bounce_tags() -> {ok, tags()} | {error, string()}.
get_bounce_tags() ->
    RequestUrl = string:join([?POSTMARK_ENDPOINT_BOUNCES, ?POSTMARK_ENDPOINT_TAGS], "/"),
    RequestHeaders = erlang_postmarkapp_request:get_default_headers(),
    case erlang_postmarkapp_request:request(get, RequestUrl, "", RequestHeaders) of
        [{headers, _}, {body, Json}] ->
            if
                is_list(Json), length(Json) > 0 ->
                    Tags = lists:map(fun (Tag) ->
                        if
                            is_binary(Tag) -> binary_to_list(Tag);
                            is_list(Tag), length(Tag) =:= 0 -> "";
                            Tag -> Tag
                        end
                    end, Json),
                    {ok, Tags};
                is_list(Json), length(Json) =:= 0 ->
                    {ok, []};
                true -> {error, "Could not properly process the API response; could not find the tags list"}
            end;
            {error, fail, Reason} -> {error, Reason};
        {error, StatusCode, _Body} ->
            Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
            {error, Message}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc processes a list of delivery stats from a JSON object into a list of <code>#postmark_bounce_stat</code> records
process_delivery_stats_response(Data) ->
    InactiveMails = erlang_postmarkapp:get_response_data("InactiveMails", Data),
    BounceStats = erlang_postmarkapp:get_attribute_value("Bounces", Data),
    if
        is_integer(InactiveMails), is_list(BounceStats) ->
            [{inactive_mails, InactiveMails}, {bounces, process_bounce_stats(BounceStats)}];
        true -> {error, "Could not properly process the API response"}
    end.

process_bounce_stats(BounceStats) ->
    process_bounce_stats(BounceStats, []).

process_bounce_stats([], Accumulator) ->
    Accumulator;

process_bounce_stats([H|T], Accumulator) ->
    if
        is_list(H), length(H) >= 2 ->
            BounceStat = #postmark_bounce_stats{
                name = erlang_postmarkapp:get_response_data("Name", H),
                count = erlang_postmarkapp:get_response_data("Count", H),
                type = erlang_postmarkapp:get_response_data("Type", H)
            },
            process_bounce_stats(T, [BounceStat | Accumulator]);
        true -> process_bounce_stats(T, Accumulator)
    end.

%% @doc converts a bounce request record to a list containing all properties that are set
bounce_request_to_list(BounceRequestRecord) when is_record(BounceRequestRecord, postmark_bounce_request) ->
    #postmark_bounce_request{count = Count, offset = Offset, bounce_type = Type, email = Email, inactive = Inactive,
        tag = Tag, message_id = MessageId, from_date = FromDate, to_date = ToDate} = BounceRequestRecord,
    Raw = [
        {<<"count">>, Count},
        {<<"offset">>, Offset},
        {<<"type">>, Type},
        {<<"inactive">>, Inactive},
        {<<"emailFilter">>, Email},
        {<<"tag">>, Tag},
        {<<"messageID">>, MessageId},
        {<<"fromdate">>, FromDate},
        {<<"todate">>, ToDate}
    ],
    Processed = lists:filter(fun erlang_postmarkapp:is_value_not_undefined/1, Raw),
    bounce_request_item_to_json(Processed);

%% @doc converts a bounce request record to a list containing all properties that are set
bounce_request_to_list(_) ->
    [].

%% @doc converts the processed list of attributes to a JSON proplist
-spec bounce_request_item_to_json(Processed::list()) -> list().
bounce_request_item_to_json(Processed) ->
    bounce_request_item_to_json(Processed, []).

%% @doc converts the processed list of attributes to a JSON proplist
-spec bounce_request_item_to_json(Processed::list(), Accumulator::list()) -> list().
bounce_request_item_to_json([], Accumulator) ->
    Accumulator;

%% @doc converts the processed list of attributes to a JSON proplist
bounce_request_item_to_json([{Key, Value}|T], Accumulator) ->
    if
        is_atom(Value) ->
            bounce_request_item_to_json(T, [
                {erlang_postmarkapp:process_json_key(Key), list_to_binary(atom_to_list(Value))} | Accumulator
            ]);
        is_list(Value) ->
            bounce_request_item_to_json(T, [
                {erlang_postmarkapp:process_json_key(Key), list_to_binary(Value)} | Accumulator
            ]);
        is_integer(Value) ->
            bounce_request_item_to_json(T, [
                {erlang_postmarkapp:process_json_key(Key), list_to_binary(integer_to_list(Value))} | Accumulator
            ]);
        true ->
            bounce_request_item_to_json(T, Accumulator)
    end.

%% @doc process a list of bounce responses, converting them to a list of #postmark_bounce{} records
process_bounces_response(Json) ->
    TotalCount = erlang_postmarkapp:get_response_data("TotalCount", Json),
    BouncesList = erlang_postmarkapp:get_attribute_value("Bounces", Json),
    if
        is_integer(TotalCount), is_list(BouncesList) ->
            [{total, TotalCount}, {bounces, lists:map(fun (Data) -> process_bounce_response(Data) end, BouncesList)}];
        true -> {error, "Could not properly process the API response"}
    end.

%% @doc converts a JSON object to a #postmark_bounce{} record
process_bounce_response(Data) when is_list(Data), length(Data) > 0 ->
    #postmark_bounce{
        id = erlang_postmarkapp:get_response_data("ID", Data),
        type = erlang_postmarkapp:get_response_data("Type", Data),
        type_code = erlang_postmarkapp:get_response_data("TypeCode", Data),
        name = erlang_postmarkapp:get_response_data("Name", Data),
        tag = erlang_postmarkapp:get_response_data("Tag", Data),
        message_id = erlang_postmarkapp:get_response_data("MessageID", Data),
        server_id = erlang_postmarkapp:get_response_data("ServerID", Data),
        description = erlang_postmarkapp:get_response_data("Description", Data),
        details = erlang_postmarkapp:get_response_data("Details", Data),
        email = erlang_postmarkapp:get_response_data("Email", Data),
        from = erlang_postmarkapp:get_response_data("From", Data),
        bounced_at = erlang_postmarkapp:get_response_data("BouncedAt", Data),
        dump_available = erlang_postmarkapp:get_response_data("DumpAvailable", Data),
        inactive = erlang_postmarkapp:get_response_data("Inactive", Data),
        can_activate = erlang_postmarkapp:get_response_data("CanActivate", Data),
        subject = erlang_postmarkapp:get_response_data("Subject", Data)
    };

%% @doc converts a JSON object to a #postmark_bounce{} record
process_bounce_response(_) ->
    [].

%% @doc processes the data received from the API
process_activate_response(Data) ->
    Message = erlang_postmarkapp:get_response_data("Message", Data),
    Bounce = erlang_postmarkapp:get_attribute_value("Bounce", Data),
    if
        Message =:= "OK", is_list(Bounce) -> {ok, process_bounce_response(Bounce)};
        Message =/= "OK" -> {error, Message};
        true -> {error, "Could not properly process the API response"}
    end.