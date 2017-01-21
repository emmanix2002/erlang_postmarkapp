%%%-------------------------------------------------------------------
%%% @author eokeke
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% @reference http://developer.postmarkapp.com/developer-api-server.html
%%% Created : 21. Jan 2017 5:59 PM
%%%-------------------------------------------------------------------
-module(erlang_postmarkapp_server).
-author("eokeke").
-include("erlang_postmarkapp.hrl").

%% API
-export([get_server/0, edit_server/1]).

-spec get_server() -> {ok, #postmark_server{}} | {error, string()}.
-spec edit_server(ServerRecord::#postmark_server{}) -> {ok, #postmark_server{}} | {error, string()}.

%%====================================================================
%% API functions
%%====================================================================

%% @spec get_server() -> {ok, #postmark_server{}} | {error, string()}.
%% @doc Get the settings for the server associated with this PostmarkClient setup; defined by the ServerToken passed in.
get_server() ->
    case erlang_postmarkapp_request:request(get, ?POSTMARK_ENDPOINT_SERVER, {string, ""}) of
        [{headers, _}, {body, Json}] ->
            case process_server_response(Json) of
                {postmark_server, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Server ->
                    {ok, Server};
                [] -> {error, "No data was found in the response"}
            end;
        {error, fail, Reason} -> {error, Reason};
        {error, StatusCode, _Body} ->
            Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
            {error, Message}
    end.

%% @spec edit_server(ServerRecord::#postmark_server{}) -> {ok, #postmark_server{}} | {error, string()}.
%% @doc Modify the associated Server.
edit_server(ServerRecord) when is_record(ServerRecord, postmark_server) ->
    Data = server_to_list(ServerRecord),
    case erlang_postmarkapp_request:request(put, ?POSTMARK_ENDPOINT_SERVER, {json, Data}) of
        [{headers, _}, {body, Json}] ->
            case process_server_response(Json) of
                {postmark_server, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Server ->
                    {ok, Server};
                [] -> {error, "No data was found in the response"}
            end;
        {error, fail, Reason} -> {error, Reason};
        {error, StatusCode, _Body} ->
            Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
            {error, Message}
    end;

%% @spec edit_server(ServerRecord::#postmark_server{}) -> {ok, #postmark_server{}} | {error, string()}.
%% @doc Modify the associated Server.
edit_server(_) ->
    {error, "You need to pass a record of type postmark_server"}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc converts a server record to a list containing all properties that are set
server_to_list(Server) when is_record(Server, postmark_server) ->
    #postmark_server{name = Name, color = Color, raw_email_enabled = RawEmailEnabled,
        smtp_api_activated = SmtpApiActivated, delivery_hook_url = DeliveryHookUrl,
        inbound_hook_url = InboundHookUrl, bounce_hook_url = BounceHookUrl,
        include_bounce_content_in_hook = IncludeBounceContent, open_hook_url = OpenHookUrl,
        post_first_open_only = PostFirstOpenOnly, track_opens = TrackOpens, track_links = TrackLinks,
        inbound_domain = InboundDomain, inbound_spam_threshold = InboundSpamThreshold} = Server,
    Raw = [
        {<<"Name">>, Name},
        {<<"Color">>, Color},
        {<<"RawEmailEnabled">>, RawEmailEnabled},
        {<<"SmtpApiActivated">>, SmtpApiActivated},
        {<<"DeliveryHookUrl">>, DeliveryHookUrl},
        {<<"InboundHookUrl">>, InboundHookUrl},
        {<<"BounceHookUrl">>, BounceHookUrl},
        {<<"IncludeBounceContentInHook">>, IncludeBounceContent},
        {<<"OpenHookUrl">>, OpenHookUrl},
        {<<"PostFirstOpenOnly">>, PostFirstOpenOnly},
        {<<"TrackOpens">>, TrackOpens},
        {<<"TrackLinks">>, TrackLinks},
        {<<"InboundDomain">>, InboundDomain},
        {<<"InboundSpamThreshold">>, InboundSpamThreshold}
    ],
    Processed = lists:filter(fun erlang_postmarkapp:is_value_not_undefined/1, Raw),
    server_item_to_json(Processed);

%% @doc converts a server record to a list containing all properties that are set
server_to_list(_) ->
    [].

%% @doc converts the processed list of attributes to a JSON proplist
server_item_to_json(Processed) ->
    server_item_to_json(Processed, []).

%% @doc converts the processed list of attributes to a JSON proplist
server_item_to_json([], Accumulator) ->
    Accumulator;

%% @doc converts the processed list of attributes to a JSON proplist
server_item_to_json([{Key, Value}|T], Accumulator) ->
    if
        Key =:= <<"TrackLinks">> ->
            TransformedValue = erlang_postmarkapp:track_links_to_string(Value),
            server_item_to_json(T, [
                {erlang_postmarkapp:process_json_key(Key), list_to_binary(TransformedValue)} | Accumulator
            ]);
        is_atom(Value) ->
            server_item_to_json(T, [
                {erlang_postmarkapp:process_json_key(Key), list_to_binary(atom_to_list(Value))} | Accumulator
            ]);
        is_list(Value) ->
            server_item_to_json(T, [
                {erlang_postmarkapp:process_json_key(Key), list_to_binary(Value)} | Accumulator
            ]);
        is_integer(Value) ->
            server_item_to_json(T, [
                {erlang_postmarkapp:process_json_key(Key), list_to_binary(integer_to_list(Value))} | Accumulator
            ]);
        true ->
            server_item_to_json(T, Accumulator)
    end.

%% @doc process a server response, converting it to a #postmark_server{} records
process_server_response(Data)  when is_list(Data), length(Data) > 0  ->
    #postmark_server{
        id = erlang_postmarkapp:get_response_data("ID", Data),
        name = erlang_postmarkapp:get_response_data("Name", Data),
        api_tokens = erlang_postmarkapp:get_response_data("ApiTokens", Data),
        server_link = erlang_postmarkapp:get_response_data("ServerLink", Data),
        color = erlang_postmarkapp:get_response_data("Color", Data),
        smtp_api_activated = erlang_postmarkapp:get_response_data("SmtpApiActivated", Data),
        raw_email_enabled = erlang_postmarkapp:get_response_data("RawEmailEnabled", Data),
        delivery_hook_url = erlang_postmarkapp:get_response_data("DeliveryHookUrl", Data),
        inbound_address = erlang_postmarkapp:get_response_data("InboundAddress", Data),
        inbound_hook_url = erlang_postmarkapp:get_response_data("InboundHookUrl", Data),
        bounce_hook_url = erlang_postmarkapp:get_response_data("BounceHookUrl", Data),
        include_bounce_content_in_hook = erlang_postmarkapp:get_response_data("IncludeBounceContentInHook", Data),
        open_hook_url = erlang_postmarkapp:get_response_data("OpenHookUrl", Data),
        post_first_open_only = erlang_postmarkapp:get_response_data("PostFirstOpenOnly", Data),
        track_opens = erlang_postmarkapp:get_response_data("TrackOpens", Data),
        track_links = erlang_postmarkapp:get_response_data("TrackLinks", Data),
        inbound_domain = erlang_postmarkapp:get_response_data("InboundDomain", Data),
        inbound_hash = erlang_postmarkapp:get_response_data("InboundHash", Data),
        inbound_spam_threshold = erlang_postmarkapp:get_response_data("InboundSpamThreshold", Data)
    };

%% @doc converts a JSON object to a #postmark_server{} record
process_server_response(_) ->
    [].
