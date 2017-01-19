-module(erlang_postmarkapp).
-author("eokeke").
-include("erlang_postmarkapp.hrl").

%% API exports
-export([
    setup/1,
    send_email/4,
    send_email/11,
    send_email_with_template/12,
    postmark_email_to_json/1,
    verify_email/1
]).

-type trackLinkStatus() :: none | html_and_text | html_only | text_only.
-type argumentValue() :: string() | undefined.
-type emailBody() :: {text, string()} | {html, string()}.
-type recordValue() :: string() | undefined | boolean() | trackLinkStatus().
-type postmarkEmail() :: {postmark_email, recordValue(), recordValue(), recordValue(), recordValue(), recordValue(),
    recordValue(), recordValue(), recordValue(), recordValue(), recordValue(), recordValue()}.
-type sendEmailResponse() :: {ok, string()} | {error, string()}.
-spec setup(ServerToken::string()) -> ok.
-spec send_email(From::string(), To::string(), Subject::string(), Body::emailBody()) -> sendEmailResponse().
-spec send_email(From::string(), To::string(), Subject::string(), HtmlBody::argumentValue(), TextBody::argumentValue(),
    Tag::string(), TrackOpens::boolean(), ReplyTo::argumentValue(), Cc::argumentValue(), Bcc::argumentValue(),
    TrackLinks::trackLinkStatus()) -> sendEmailResponse().
-spec send_email(PostmarkEmail::postmarkEmail()) -> sendEmailResponse().
-spec send_email_with_template(From::string(), To::string(), TemplateId::string(), TemplateModel::list(),
    InlineCss::boolean(), Tag::argumentValue(), TrackOpens::boolean(), ReplyTo::argumentValue(), Cc::argumentValue(),
    Bcc::argumentValue(), Headers::list(), TrackLinks::trackLinkStatus()) -> ok.
-spec postmark_email_to_json(PostmarkEmail::postmarkEmail()) -> list().
-spec get_attribute_value(Attribute::string(), Proplist::list()) -> list() | binary().
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

%% @spec send_email(From::string(), To::string(), Subject::string(), HtmlBody::argumentValue(), TextBody::argumentValue(), Tag::string(), TrackOpens::boolean(), ReplyTo::argumentValue(), Cc::argumentValue(), Bcc::argumentValue(), TrackLinks::trackLinkStatus())
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
                    MessageId = binary_to_list(get_attribute_value("MessageID", Json)),
                    ErrorCode = get_attribute_value("ErrorCode", Json),
                    Message = binary_to_list(get_attribute_value("Message", Json)),
                    if
                        length(MessageId) > 0 -> {ok, MessageId};
                        ErrorCode =/= 0 -> {error, Message};
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

send_email_with_template(From, To, TemplateId, TemplateModel, InlineCss, Tag, TrackOpens, ReplyTo, Cc, Bcc, Headers, TrackLinks) -> ok.


%%====================================================================
%% Internal functions
%%====================================================================
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
        text_only -> "TextOnly"
    end.

%% @spec spec postmark_email_to_json(PostmarkEmail::postmarkEmail()) -> list()
%% @doc converts a postmark email record to the appropriate JSON data array
postmark_email_to_json(PostmarkEmail) when is_record(PostmarkEmail, postmark_email) ->
    #postmark_email{from = From, to = To, subject = Subject, html = Html, text = Text, tag = Tag,
        track_opens = TrackOpens, reply_to = ReplyTo, cc = Cc, bcc = Bcc, track_links = TrackLinks} = PostmarkEmail,
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
        {<<"TrackOpens">>, TrackOpens},
        {<<"TrackLinks">>, track_links_to_string(TrackLinks)}
    ],
    Processed = lists:filter(fun ({_Key, Value}) ->
        case Value of
            undefined -> false;
            Value -> true
        end
                             end, Raw),
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
        {Key, Value} ->
            if
                is_atom(Value) -> email_item_to_json(T, [{Key, list_to_binary(atom_to_list(Value))} | Accumulator]);
                is_list(Value) -> email_item_to_json(T, [{Key, list_to_binary(Value)} | Accumulator]);
                true -> email_item_to_json(T, Accumulator)
            end;
        _ -> email_item_to_json(T, Accumulator)
    end.

%% @spec get_attribute_value(Attribute::string(), Proplist::list()) -> term()
%% @doc returns the value for an attribute
get_attribute_value(Attribute, Proplist) ->
    proplists:get_value(list_to_binary(Attribute), Proplist).