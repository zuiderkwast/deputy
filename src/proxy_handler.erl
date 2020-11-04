%% Copyright (c) 2020, Viktor Söderqvist <viktor.soderqvist@est.tech>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(proxy_handler).

-behaviour(cowboy_handler).
-export([init/2, terminate/3]).

-behaviour(cowboy_tunnel).
-export([tunnel_init/1, tunnel_info/2, tunnel_handle/3]).

init(Req = #{method := <<"CONNECT">>, host := Host, port := Port}, _State) ->
        %% @todo Prevent connect loop
        io:format("CONNECT ~s:~p~n", [Host, Port]),
        {cowboy_tunnel, Req, {Host, Port}};

init(Req0, State) ->

    #{method  := Method,
      host    := Host,
      port    := Port,
      path    := Path0,
      qs      := Qs,
      headers := Headers} = Req0,

    io:format("~s ~s:~p~s~s~n", [Method, Host, Port, Path0, Qs]),

    GunOpts = #{transport => tcp},
    {ok, GunPid} = gun:open(binary_to_list(Host), Port, GunOpts),
    {ok, Body, Req1} = cowboy_req_body(Req0, <<>>),
    Path = case Qs of
               <<>> -> Path0;
               _    -> <<Path0/binary, "?", Qs/binary>>
           end,
    GunRef = gun:request(GunPid, Method, Path, Headers, Body),
    case gun:await(GunPid, GunRef) of
        {response, Fin, Status, RespHeaders} ->
            RespBody =
                case Fin of
                    fin ->
                        <<>>;
                    nofin ->
                        {ok, RespBody0} = gun:await_body(GunPid, GunRef),
                        RespBody0
                end,
            Req2 = cowboy_req:reply(Status, maps:from_list(RespHeaders),
                                    RespBody, Req1),
            gun:close(GunPid),
            {ok, Req2, State};
        GunError ->
            io:format("GunError ~p\n", [GunError]),
            Req2 = cowboy_req:reply(500,
                                    #{<<"content-type">> => <<"text/plain">>},
                                    io_lib:format("~p", [GunError]),
                                    Req1),
            gun:close(GunPid),
            {ok, Req2, State}
    end.

cowboy_req_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {more, Data, Req} ->
            cowboy_req_body(Req, <<Acc/binary, Data/binary>>);
        {ok, Data, Req} when Acc =/= <<>> ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        Ok ->
            Ok
    end.

tunnel_init({Host, Port}) ->
    case gen_tcp:connect(binary_to_list(Host), Port,
                         [{active, once}, {packet, raw}, binary,
                          {exit_on_close, false}]) of
            {ok, Sock} ->
                    {[], Sock};
            {error, _Reason} ->
                    io:format(user, "Connect ~s:~p ==> error ~p~n",
                              [Host, Port, _Reason]),
                    {[stop], []}
    end.

tunnel_handle(IsFin, Data, Sock) ->
        case gen_tcp:send(Sock, Data) of
                ok when IsFin =:= fin ->
                        gen_tcp:shutdown(Sock, write),
                        {[], Sock};
                ok ->
                        {[], Sock};
                _Error ->
                        {[stop], Sock}
        end.

tunnel_info({tcp, Sock, Data}, Sock) ->
        ok = inet:setopts(Sock, [{active, once}]),
        {[{data, Data}], Sock};
tunnel_info({tcp_closed, Sock}, Sock) ->
        {[stop], Sock};
tunnel_info({tcp_error, Sock, _Reason}, Sock) ->
        {[stop], Sock};
tunnel_info(StrayMessage, Sock) ->
        logger:info("Stray message: ~p", [StrayMessage]),
        {[], Sock}.

terminate(_Reason, _Req, #{}) ->
        %% No tunnel
        ok;
terminate(_Reason, _Req, []) ->
        %% Tunnel socket not connected
        ok;
terminate(_Reason, _Req, Sock) ->
        gen_tcp:close(Sock).
