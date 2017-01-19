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
-define(POSTMARK_ENDPOINT_EMAIL_BATCH, string:join(["email", ?POSTMARK_ENDPOINT_EMAIL], "/")).
-define(POSTMARK_ETS_TABLE, postmark_tbl).
-define(POSTMARK_ETS_TOKEN_KEY, server_token).
-record(postmark_email, {from, to, subject, html, text, tag, track_opens=true, reply_to, cc, bcc, track_links}).