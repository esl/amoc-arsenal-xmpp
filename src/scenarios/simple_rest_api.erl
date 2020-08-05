%%==============================================================================
%% @copyright 2019-2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%% @end
%%
%% @doc
%% In this scenario users are sending messages using the REST client API.
%%
%% == User steps: ==
%%
%% 0. Choose a role based on the user ID. Half of the users send messages via
%% the REST API, the other half receives them through XMPP.
%%
%% === REST users ===
%% 1. Send one message via the REST API.
%%
%% === XMPP users ===
%% 1. Connect to the XMPP host.
%%
%% 2. Send presence `available'.
%%
%% 3. Wait for incoming messages and log them.
%%
%% @end
%%==============================================================================
-module(simple_rest_api).

-behaviour(amoc_scenario).

-export([start/1]).
-export([init/0]).

-include_lib("kernel/include/logger.hrl").

-spec init() -> ok.
init() ->
    http_req:start(),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    do_start(xmpp_or_rest(MyId), MyId).

do_start(rest, MyId) ->
    AuthHeader = auth_header(MyId),
    send_message(AuthHeader, MyId - 1),
    ok;
do_start(xmpp, MyId) ->
    ExtraProps = [{received_stanza_handlers, [fun log_message/2]}],

    {ok, Client, _} = amoc_xmpp:connect_or_exit(MyId, ExtraProps),

    escalus_session:send_presence_available(Client),

    escalus_connection:wait_forever(Client),

    escalus_connection:stop(Client).

log_message(_Client, Stanza) ->
    ?LOG_WARNING("~p", [Stanza]),
    true.

send_message(AuthHeader, Id) ->
    Msg = #{to => amoc_xmpp_users:make_jid(Id),
            body => <<"Hello, It's me">>
           },
    Headers = [AuthHeader,
               {<<"content-type">>, <<"application/json">>}],
    R = http_req:post_request("https://localhost:8089", <<"/api/messages">>, Headers, jiffy:encode(Msg)),
    ?LOG_WARNING("~p", [R]).

auth_header(Id) ->
    Password = <<"password_", (integer_to_binary(Id))/binary>>,
    User = amoc_xmpp_users:make_jid(Id),
    UserAndPass = <<User/binary, ":", Password/binary>>,
    Base64  = base64:encode(UserAndPass),
    Basic = <<"basic ",Base64/binary>>,
    {<<"authorization">>, Basic}.

xmpp_or_rest(MyId) ->
    Id = MyId rem 2 + 1,
    element(Id, {rest, xmpp}).
