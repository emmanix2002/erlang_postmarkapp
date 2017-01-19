erlang_postmarkapp
=====

An erlang library for consuming the Postmark mail service api. 
It was inspired by the official Postmark PHP package 
[https://github.com/wildbit/postmark-php](https://github.com/wildbit/postmark-php).   
Right now, it does only one thing: send emails :)

Build
-----

    $ rebar3 compile

What does it support
------------
At the moment, the library only supports _**sending out emails**_. It supports sending in these forms:   

- Sending a single email `send_email`
- Sending in batches `send_email_batch`
- Sending using templates `send_email_with_template`

Installation
--------
You can add it to your `rebar.config` like so:    

```erlang
{deps, [
       ...
       {erlang_postmarkapp, "1.*", {git, "https://github.com/emmanix2002/erlang_postmarkapp.git", {branch, "master"}}}
]}.
```

Usage
-------
To use the library, you need to set it up by providing the `server token`. 
See an example below:    

```erlang
send_text_mail() ->
    ServerToken = "your server token here",
    erlang_postmarkapp:setup(ServerToken),
    Body = {text, "Hello World!"},
    case erlang_postmarkapp:send_email("signature@domain.com", "recipient@example.com", "A good example", Body) of
        {ok, MessageId} -> io:format("Successfully sent email with MessageID ~p~n", [MessageId]);
        {error, Reason} -> io:format("Message sending failed because ~p~n", [Reason])
    end.
```

- `setup(ServerToken)`: sets the server token that will be used to authenticate with the Postman API.
- `send_email*()`: these functions allow you to send an email.    

**NOTE**: based on the information on the Postman documentation, batch emails are limited to a maximum of 
_**500**_ messages (the library enforces this); while for single emails, you can have at most _**50**_ email 
addresses from a combination of all: `To, Cc, and Bcc` addresses.