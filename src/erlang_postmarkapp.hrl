%%%-------------------------------------------------------------------
%%% @author eokeke
%%% @copyright (C) 2017
%%% @doc
%%% This header file contains <code>macro</code> and <code>record</code> definitions.
%%% @end
%%% Created : 18. Jan 2017 1:42 PM
%%%-------------------------------------------------------------------
-author("eokeke").
-define(POSTMARK_REST_URL, "https://api.postmarkapp.com").
-define(POSTMARK_REQUEST_OPTS, [{timeout, 30000}, {ssl, [{verify, verify_none}]}]).
-define(POSTMARK_CONTENT_TYPE, "application/json").
-define(POSTMARK_ENDPOINT_EMAIL, "email").
-define(POSTMARK_ENDPOINT_EMAIL_BATCH, string:join([?POSTMARK_ENDPOINT_EMAIL, "batch"], "/")).
-define(POSTMARK_ENDPOINT_EMAIL_WITH_TEMPLATE, string:join([?POSTMARK_ENDPOINT_EMAIL, "withTemplate"], "/")).
-define(POSTMARK_ENDPOINT_BOUNCES, "bounces").
-define(POSTMARK_ENDPOINT_ACTIVATE, "activate").
-define(POSTMARK_ENDPOINT_DUMP, "dump").
-define(POSTMARK_ENDPOINT_TAGS, "tags").
-define(POSTMARK_ENDPOINT_DELIVERY_STATS, "deliverystats").
-define(POSTMARK_ENDPOINT_SERVER, "server").
-define(POSTMARK_BATCH_MAX_RECIPIENTS, 500).
-define(POSTMARK_ETS_TABLE, postmark_tbl).
-define(POSTMARK_ETS_ACCOUNT_TOKEN_KEY, account_token).
-define(POSTMARK_ETS_SERVER_TOKEN_KEY, server_token).

-type trackLinkStatus() :: none | html_and_text | html_only | text_only.
-type optionalValue() :: string() | undefined.
-type optionalListValue() :: list() | undefined.
-type listAttachments() :: [#postmark_attachment{}].
-type serverColor() :: purple | blue | turqoise | green | red | yellow | grey.

%% a Postmark email record
-record(postmark_email, {
    from                :: string(),
    to                  :: string(),
    subject             :: string(),
    html                :: optionalValue(),
    text                :: optionalValue(),
    tag                 :: optionalValue(),
    track_opens=true    :: boolean(),
    reply_to            :: optionalValue(),
    cc                  :: optionalValue(),
    bcc                 :: optionalValue(),
    track_links         :: trackLinkStatus(),
    template_id         :: optionalValue(),
    template_model      :: optionalListValue(),
    attachments         :: listAttachments(),
    inline_css=true     :: boolean()
}).

%% the record representing a Postmark API response
-record(postmark_send_response, {
    message_id  :: string(),
    error_code  :: integer(),
    message     :: string()
}).

%% a record that encapsulates a bounce request
-record(postmark_bounce_request, {
    count=500       :: integer(),
    offset=0        :: integer(),
    bounce_type     :: string(),
    email           :: string(),
    inactive        :: boolean(),
    tag             :: string(),
    message_id      :: string(),
    from_date       :: string(),
    to_date         :: string()
}).

%% represents attachment record for attaching files/media to email
-record(postmark_attachment, {
    name :: binary(),
    content_type :: binary(),
    content :: binary() %% Base64 binary
}).

%% represents a bounce type record returned after a call to get delivery stats, which returns a list of bounce types
-record(postmark_bounce_stats, {
    type    :: optionalValue(),
    name    :: string(),
    count   :: integer()
}).

%% a record that represents an email bounce
-record(postmark_bounce, {
    id              :: integer(),
    type            :: string(),
    type_code       :: integer(),
    name            :: string(),
    tag             :: optionalValue(),
    message_id      :: optionalValue(),
    server_id       :: optionalValue(),
    description     :: optionalValue(),
    details         :: optionalValue(),
    email           :: string(),
    from            :: string(),
    bounced_at      :: string(),
    dump_available  :: boolean(),
    inactive        :: boolean(),
    can_activate    :: boolean(),
    subject         :: string()
}).

%% represents a server in the API; it can be used also for making a request with the edit_server function
-record(postmark_server, {
    id                              :: integer(),
    name                            :: string(),
    api_tokens=[]                   :: list(),
    server_link                     :: string(),
    color                           :: serverColor(),
    smtp_api_activated              :: boolean(),
    raw_email_enabled               :: boolean(),
    delivery_hook_url               :: string(),
    inbound_address                 :: string(),
    inbound_hook_url                :: string(),
    bounce_hook_url                 :: string(),
    include_bounce_content_in_hook  :: boolean(),
    open_hook_url                   :: boolean(),
    post_first_open_only            :: boolean(),
    track_opens                     :: boolean(),
    track_links                     :: trackLinkStatus(),
    inbound_domain                  :: string(),
    inbound_hash                    :: string(),
    inbound_spam_threshold          :: integer()
}).