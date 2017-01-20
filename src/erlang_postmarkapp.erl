%%%-------------------------------------------------------------------
%%% @author eokeke
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% @reference http://developer.postmarkapp.com/developer-api-email.html
%%% Created : 20. Jan 2017 10:14 AM
%%%-------------------------------------------------------------------
-module(erlang_postmarkapp).
-author("eokeke").
-include("erlang_postmarkapp.hrl").

%% API exports
-export([
    setup/1,
    send_email/1,
    send_email/4,
    send_email/11,
    send_email_with_template/10,
    postmark_email_to_json/1,
    verify_email/1,
    send_email_batch/1,
    send_email_with_template/1,
    get_attribute_value/2,
    get_response_data/2,
    is_value_not_undefined/1,
    process_json_key/1
]).

-type emailBody() :: {text, string()} | {html, string()}.
-type postmarkEmail() :: #postmark_email{}.
-type postmarkEmails() :: [postmarkEmail()].
-type sendEmailResponse() :: {ok, string()} | {error, string()}.
-spec setup(ServerToken::string()) -> ok.
-spec send_email(From::string(), To::string(), Subject::string(), Body::emailBody()) -> sendEmailResponse().
-spec send_email(From::string(), To::string(), Subject::string(), HtmlBody::argumentValue(), TextBody::argumentValue(),
    Tag::string(), TrackOpens::boolean(), ReplyTo::argumentValue(), Cc::argumentValue(), Bcc::argumentValue(),
    TrackLinks::trackLinkStatus()) -> sendEmailResponse().
-spec send_email(PostmarkEmail::#postmark_email{}) -> sendEmailResponse().
-spec send_email_with_template(From::string(), To::string(), TemplateId::string(), TemplateModel::list(),
    Tag::argumentValue(), TrackOpens::boolean(), ReplyTo::argumentValue(), Cc::argumentValue(),
    Bcc::argumentValue(), TrackLinks::trackLinkStatus()) -> sendEmailResponse().
-spec send_email_with_template(PostmarkEmail::#postmark_email{}) -> sendEmailResponse().
-spec send_email_batch(PostmarkEmailList::postmarkEmails()) -> sendEmailResponse().
-spec postmark_email_to_json(PostmarkEmail::postmarkEmail()) -> list().
-spec get_attribute_value(Attribute::string(), Proplist::list()) -> list() | binary() | undefined.
-spec verify_email(PostmarkEmail::postmarkEmail()) -> boolean().

%%====================================================================
%% API functions
%%====================================================================

%% @spec setup(ServerToken::string()) -> ok
%% @doc sets up the environment for making postmark requests
setup(ServerToken) ->
    ets:new(?POSTMARK_ETS_TABLE, [set, named_table]),
    ets:insert(?POSTMARK_ETS_TABLE, {?POSTMARK_ETS_TOKEN_KEY, ServerToken}),
    inets:start(),
    ssl:start(),
    ok.

%% @spec send_email(From::string(), To::string(), Subject::string(), Body::emailBody()) -> ok
%% @doc sends a single email
send_email(From, To, Subject, Body) ->
    case Body of
        {html, HtmlBody} ->
            send_email(From, To, Subject, HtmlBody, undefined, undefined, true, undefined, undefined, undefined, none);
        {text, TextBody} ->
            send_email(From, To, Subject, undefined, TextBody, undefined, true, undefined, undefined, undefined, none)
    end.

%% @spec send_email(From::string(), To::string(), Subject::string(), HtmlBody::argumentValue(), TextBody::argumentValue(), Tag::string(), TrackOpens::boolean(), ReplyTo::argumentValue(), Cc::argumentValue(), Bcc::argumentValue(), TrackLinks::trackLinkStatus()) -> sendEmailResponse()
%% @doc sends a single email
send_email(From, To, Subject, HtmlBody, TextBody, Tag, TrackOpens, ReplyTo, Cc, Bcc, TrackLinks) ->
    Email = #postmark_email{from = From, to = To, subject = Subject, html = HtmlBody, text = TextBody, tag = Tag,
        track_opens = TrackOpens, reply_to = ReplyTo, cc = Cc, bcc = Bcc, track_links = TrackLinks},
    send_email(Email).

%% @spec send_email(PostmarkEmail::postmarkEmail()) -> sendEmailResponse()
%% @doc sends a single email using the email record
send_email(PostmarkEmail) when is_record(PostmarkEmail, postmark_email) ->
    case verify_email(PostmarkEmail) of
        true ->
            PostData = postmark_email_to_json(PostmarkEmail),
            case erlang_postmarkapp_request:request(post, ?POSTMARK_ENDPOINT_EMAIL, {json, PostData}) of
                [{headers, _}, {body, Json}] ->
                    SendResponse = process_send_response(Json),
                    if
                        length(SendResponse#postmark_send_response.message_id) > 0 ->
                            {ok, SendResponse#postmark_send_response.message_id};
                        SendResponse#postmark_send_response.error_code =/= 0 ->
                            {error, SendResponse#postmark_send_response.message};
                        true -> {error, unknown}
                    end;
                {error, fail, Reason} -> {error, Reason};
                {error, StatusCode, _Body} ->
                    Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
                    {error, Message}
            end;
        {false, Reason} -> {error, Reason}
    end;

%% @spec send_email(PostmarkEmail::postmarkEmail()) -> sendEmailResponse()
%% @doc sends a single email using the email record
send_email(_) ->
    throw("PostmarkEmail must be a record of type postmark_email").

%% @spec send_email_batch(PostmarkEmailList::postmarkEmails()) -> sendEmailResponse().
%% @doc Send multiple emails as a batch; sends a list of postmark_email records as an email to the required addresses.
send_email_batch(PostmarkEmailList) when is_list(PostmarkEmailList), length(PostmarkEmailList) > 0,
    length(PostmarkEmailList) =< ?POSTMARK_BATCH_MAX_RECIPIENTS ->
    Payload = collect_emails(PostmarkEmailList),
    if
        length(Payload) > 0 ->
            case erlang_postmarkapp_request:request(post, ?POSTMARK_ENDPOINT_EMAIL_BATCH, {json, Payload}) of
                [{headers, _}, {body, Json}] ->
                    SendResponses = process_batch_send_response(Json),
                    io:format("Responses: ~p~n", [SendResponses]),
                    SuccessfulSends = lists:filter(fun (SendResponse) ->
                        case SendResponse#postmark_send_response.message_id of
                            undefined -> false;
                            _ -> true
                        end
                    end, SendResponses),
                    if
                        length(SuccessfulSends) =:= length(Payload) ->
                            {ok, "Successfully sent all messages"};
                        length(SuccessfulSends) > 0 ->
                            {ok,
                                string:join([
                                    "Successfully sent",
                                    integer_to_list(length(SuccessfulSends)),
                                    "out of ",
                                    length(Payload),
                                    "emails"
                                ], " ")};
                        true -> {error, "Email batch sending failed"}
                    end;
                {error, fail, Reason} -> {error, Reason};
                {error, StatusCode, _Body} ->
                    Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
                    {error, Message}
            end;
        true ->
            {error, "You provided a list, but there seem to be no valid postmark_email records in it"}
    end;

%% @spec send_email_batch(PostmarkEmailList::postmarkEmails()) -> sendEmailResponse().
%% @doc Send multiple emails as a batch; sends a list of postmark_email records as an email to the required addresses.
send_email_batch(PostmarkEmailList) when is_list(PostmarkEmailList),
    length(PostmarkEmailList) > ?POSTMARK_BATCH_MAX_RECIPIENTS ->
    {error,
        string:join([
            "You can only send up to a maximum of",
            ?POSTMARK_BATCH_MAX_RECIPIENTS,
            "recipients per batch"
        ], " ")
    };

%% @spec send_email_batch(PostmarkEmailList::postmarkEmails()) -> sendEmailResponse().
%% @doc Send multiple emails as a batch; sends a list of postmark_email records as an email to the required addresses.
send_email_batch(_) ->
    {error, "You need to provide a list of postmark_email records to be sent in a batch"}.

%% @spec send_email_with_template(From::string(), To::string(), TemplateId::string(), TemplateModel::list(), Tag::argumentValue(), TrackOpens::boolean(), ReplyTo::argumentValue(), Cc::argumentValue(), Bcc::argumentValue(), TrackLinks::trackLinkStatus()) -> sendEmailResponse()
%% @doc Send an email using a template.
send_email_with_template(From, To, TemplateId, TemplateModel, Tag, TrackOpens, ReplyTo, Cc, Bcc, TrackLinks) ->
    Email = #postmark_email{from = From, to = To, tag = Tag, track_opens = TrackOpens, reply_to = ReplyTo, cc = Cc,
        bcc = Bcc, track_links = TrackLinks, template_id = TemplateId, template_model = TemplateModel},
    send_email_with_template(Email).

%% @spec send_email_with_template(PostmarkEmail::postmarkEmail()) -> sendEmailResponse()
%% @doc Send an email using a template.
send_email_with_template(PostmarkEmail) when is_record(PostmarkEmail, postmark_email) ->
    case verify_email(PostmarkEmail) of
        true ->
            PostData = postmark_email_to_json(PostmarkEmail),
            case erlang_postmarkapp_request:request(post, ?POSTMARK_ENDPOINT_EMAIL_WITH_TEMPLATE, {json, PostData}) of
                [{headers, _}, {body, Json}] ->
                    SendResponse = process_send_response(Json),
                    if
                        length(SendResponse#postmark_send_response.message_id) > 0 ->
                            {ok, SendResponse#postmark_send_response.message_id};
                        SendResponse#postmark_send_response.error_code =/= 0 ->
                            {error, SendResponse#postmark_send_response.message};
                        true -> {error, unknown}
                    end;
                {error, fail, Reason} -> {error, Reason};
                {error, StatusCode, _Body} ->
                    Message = string:join(["The request failed due to a ", integer_to_list(StatusCode), "error"], " "),
                    {error, Message}
            end;
        {false, Reason} -> {error, Reason}
    end;

%% @spec send_email_with_template(PostmarkEmail::postmarkEmail()) -> sendEmailResponse()
%% @doc sends a single email using the email record
send_email_with_template(_) ->
    throw("PostmarkEmail must be a record of type postmark_email").

%% @doc returns the value of a key in a proplist converting binaries to strings when necessary
get_response_data(Key, Data) ->
    case get_attribute_value(Key, Data) of
        undefined -> undefined;
        Value ->
            if
                is_binary(Value) -> binary_to_list(Value);
                true -> Value
            end
    end.
%%====================================================================
%% Internal functions
%%====================================================================

%% @doc process a list of email responses, converting them to a list of #postmark_send_response records
process_batch_send_response(DataList) when is_list(DataList), length(DataList) > 0 ->
    lists:map(fun (Data) -> process_send_response(Data) end, DataList);

%% @doc process a list of email responses, converting them to a list of #postmark_send_response records
process_batch_send_response(_) ->
    [].

%% @doc converts a JSON object to a #postmark_send_response{} record
process_send_response(Data) when is_list(Data), length(Data) > 0 ->
    #postmark_send_response{
        message_id = get_response_data("MessageID", Data),
        error_code = get_response_data("ErrorCode", Data),
        message = get_response_data("Message", Data)
    };

%% @doc converts a JSON object to a #postmark_send_response{} record
process_send_response(_) ->
    [].

%% @doc processes a list of #postmark_email{} records, returning a list of processed email JSON objects
collect_emails(EmailList) ->
    collect_emails(EmailList, []).

%% @doc processes a list of #postmark_email{} records, returning a list of processed email JSON objects
collect_emails([], Accumulator) ->
    Accumulator;

%% @doc processes a list of #postmark_email{} records, returning a list of processed email JSON objects
collect_emails([H|T], Accumulator) ->
    case H of
        {postmark_email, _, _, _, _, _, _, _, _, _, _, _, _, _, _} ->
            collect_emails(T, [postmark_email_to_json(H) | Accumulator]);
        _ ->
            collect_emails(T, Accumulator)
    end.

%% @spec verify_email(PostmarkEmail::postmarkEmail()) -> boolean().
%% @doc checks the email for some conditions that show if it should be sent or not
verify_email(PostmarkEmail) when is_record(PostmarkEmail, postmark_email) ->
    #postmark_email{to = To, cc = Cc, bcc = Bcc} = PostmarkEmail,
    ToAddresses = lists:filter(fun (Value) ->
        case Value of
            undefined -> false;
            Value -> true
        end
                               end, [To, Cc, Bcc]),
    List = lists:flatmap(fun (Tos) -> string:tokens(Tos, ", ") end, ToAddresses),
    if
        length(List) > 50 -> {false, "There can only be 50 max recipients per message"};
        length(List) =:= 0 -> {false, "No email recipients specified"};
        true -> true
    end;

%% @spec verify_email(PostmarkEmail::postmarkEmail()) -> boolean().
%% @doc checks the email for some conditions that show if it should be sent or not
verify_email(_) ->
    {false, "No a valid postmark email record"}.

%% @doc converts the track links option to the string value
track_links_to_string(TrackLinkStatus) ->
    case TrackLinkStatus of
        none -> "None";
        html_and_text -> "HtmlAndText";
        html_only -> "HtmlOnly";
        text_only -> "TextOnly";
        undefined -> "None"
    end.

%% @spec spec postmark_email_to_json(PostmarkEmail::postmarkEmail()) -> list()
%% @doc converts a postmark email record to the appropriate JSON data array
postmark_email_to_json(PostmarkEmail) when is_record(PostmarkEmail, postmark_email) ->
    #postmark_email{from = From, to = To, subject = Subject, html = Html, text = Text, tag = Tag,
        track_opens = TrackOpens, reply_to = ReplyTo, cc = Cc, bcc = Bcc, track_links = TrackLinks,
        template_id = TemplateId, template_model = TemplateModel, inline_css = InlineCss} = PostmarkEmail,
    Raw = [
        {<<"From">>, From},
        {<<"To">>, To},
        {<<"Cc">>, Cc},
        {<<"Bcc">>, Bcc},
        {<<"Subject">>, Subject},
        {<<"Tag">>, Tag},
        {<<"HtmlBody">>, Html},
        {<<"TextBody">>, Text},
        {<<"ReplyTo">>, ReplyTo},
        {<<"TemplateId">>, TemplateId},
        {<<"TemplateModel">>, TemplateModel},
        {<<"InlineCss">>, InlineCss},
        {<<"TrackOpens">>, TrackOpens},
        {<<"TrackLinks">>, track_links_to_string(TrackLinks)}
    ],
    Processed = lists:filter(fun is_value_not_undefined/1, Raw),
    email_item_to_json(Processed);

%% @spec spec postmark_email_to_json(PostmarkEmail::postmarkEmail()) -> list()
%% @doc converts a postmark email record to the appropriate JSON data array
postmark_email_to_json(_PostmarkEmail) ->
    [].

%% @spec email_item_to_json(Processed::list()) -> list().
%% @doc converts the processed list of attributes to a JSON proplist
email_item_to_json(Processed) ->
    email_item_to_json(Processed, []).

%% @spec email_item_to_json(Processed::list(), Accumulator::list()) -> list().
%% @doc converts the processed list of attributes to a JSON proplist
email_item_to_json([], Accumulator) ->
    Accumulator;

%% @spec email_item_to_json(Processed::list(), Accumulator::list()) -> list().
%% @doc converts the processed list of attributes to a JSON proplist
email_item_to_json([H|T], Accumulator) ->
    case H of
        {<<"TemplateModel">>, Model} ->
            ModelData = email_item_to_json(Model, []),
            email_item_to_json(T, [{<<"TemplateModel">>, ModelData} | Accumulator]);
        {Key, Value} ->
            if
                is_atom(Value) ->
                    email_item_to_json(T, [{process_json_key(Key), list_to_binary(atom_to_list(Value))} | Accumulator]);
                is_list(Value) ->
                    email_item_to_json(T, [{process_json_key(Key), list_to_binary(Value)} | Accumulator]);
                true ->
                    email_item_to_json(T, Accumulator)
            end
    end.

%% @spec get_attribute_value(Attribute::string(), Proplist::list()) -> list() | binary() | undefined.
%% @doc returns the value for an attribute
get_attribute_value(Attribute, Proplist) ->
    proplists:get_value(list_to_binary(Attribute), Proplist).

%% @doc converts a json object key to a binary
process_json_key(Key) when not is_binary(Key) ->
    if
        is_list(Key) -> list_to_binary(Key);
        is_atom(Key) -> list_to_binary(atom_to_list(Key));
        true -> Key
    end;

%% @doc converts a json object key to a binary
process_json_key(Key) -> Key.

%% @doc checks that a value in a tuple of the form {Key, Value} is not the atom `undefined`
is_value_not_undefined({_, Value}) ->
    case Value of
        undefined -> false;
        _ -> true
    end.