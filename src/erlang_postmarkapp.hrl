%%%-------------------------------------------------------------------
%%% @author eokeke
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
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
-define(POSTMARK_BATCH_MAX_RECIPIENTS, 500).
-define(POSTMARK_ETS_TABLE, postmark_tbl).
-define(POSTMARK_ETS_TOKEN_KEY, server_token).

-type trackLinkStatus() :: none | html_and_text | html_only | text_only.
-type argumentValue() :: string() | undefined.
-type listArgumentValue() :: list() | undefined.

%% a Postmark email record
-record(postmark_email, {
    from                :: string(),
    to                  :: string(),
    subject             :: string(),
    html                :: argumentValue(),
    text                :: argumentValue(),
    tag                 :: argumentValue(),
    track_opens=true    :: boolean(),
    reply_to            :: argumentValue(),
    cc                  :: argumentValue(),
    bcc                 :: argumentValue(),
    track_links         :: trackLinkStatus(),
    template_id         :: argumentValue(),
    template_model      :: listArgumentValue(),
    inline_css=true     :: boolean()
}).

%% the record representing a Postmark API response
-record(postmark_send_response, {
    message_id  :: string(),
    error_code  :: integer(),
    message     :: string()
}).

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

%% represents a bounce type record returned after a call to get delivery stats, which returns a list of bounce types
-record(postmark_bounce_stats, {
    type    :: argumentValue(),
    name    :: string(),
    count   :: integer()
}).

-record(postmark_bounce, {
    id              :: integer(),
    type            :: string(),
    type_code       :: integer(),
    name            :: string(),
    tag             :: argumentValue(),
    message_id      :: argumentValue(),
    server_id       :: argumentValue(),
    description     :: argumentValue(),
    details         :: argumentValue(),
    email           :: string(),
    from            :: string(),
    bounced_at      :: string(),
    dump_available  :: boolean(),
    inactive        :: boolean(),
    can_activate    :: boolean(),
    subject         :: string()
}).