%%%-------------------------------------------------------------------
%%% @author eokeke
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2017 1:22 PM
%%%-------------------------------------------------------------------
-module(single_email).
-author("eokeke").
-include("../src/erlang_postmarkapp.hrl").

%% API
-export([send_text_mail/0, send_html_email/0, send_using_email_record/0, send_with_template/0]).

send_text_mail() ->
    ServerToken = "your server token here",
    erlang_postmarkapp:setup(ServerToken),
    Body = {text, "Hello World!"},
    case erlang_postmarkapp:send_email("signature@domain.com", "recipient@example.com", "A good example", Body) of
        {ok, MessageId} -> io:format("Successfully sent email with MessageID ~p~n", [MessageId]);
        {error, Reason} -> io:format("Message sending failed because ~p~n", [Reason])
    end.

send_html_email() ->
    ServerToken = "your server token here",
    erlang_postmarkapp:setup(ServerToken),
    Body = {html, "Hello World!"},
    case erlang_postmarkapp:send_email("signature@domain.com", "recipient@example.com", "A good example", Body) of
        {ok, MessageId} -> io:format("Successfully sent email with MessageID ~p~n", [MessageId]);
        {error, Reason} -> io:format("Message sending failed because ~p~n", [Reason])
    end.

send_using_email_record() ->
    ServerToken = "your server token here",
    erlang_postmarkapp:setup(ServerToken),
    Email = #postmark_email{from = "signature@domain.com", to = "recipient@example.com", subject = "A good example",
        html = "<strong>Hi there!</strong><p>Hello World!</p>"},
    case erlang_postmarkapp:send_email(Email) of
        {ok, MessageId} -> io:format("Successfully sent email with MessageID ~p~n", [MessageId]);
        {error, Reason} -> io:format("Message sending failed because ~p~n", [Reason])
    end.

send_with_template() ->
    ServerToken = "your server token here",
    erlang_postmarkapp:setup(ServerToken),
    Model = [
        {product_name, "Example Publishers Co"},
        {name, "Forgetful User"},
        {sender_name, "Example LLC."},
        {action_url, "http://example.com/reset-password?token=xdjfvhdcpf5pwg53iutsipj"}
    ],
    case erlang_postmarkapp:send_email_with_template(
        "signature@domain.com",
        "recipient@example.com",
        "template_id",
        Model,
        "password-recovery",
        true,
        undefined,
        undefined,
        undefined,
        none
    ) of
        {ok, MessageId} -> io:format("Successfully sent email with MessageID ~p~n", [MessageId]);
        {error, Reason} -> io:format("Message sending failed because ~p~n", [Reason])
    end.
