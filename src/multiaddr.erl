-module(multiaddr).

-define(IP4, 16#04).
-define(IP6, 16#29).
-define(TCP, 16#06).
-define(UDP, 16#11).
-define(DCCP, 16#21).
-define(SCTP, 16#84).
-define(UDT, 16#012D).
-define(UTP, 16#012E).
-define(IPFS, 16#2A).
-define(HTTP, 16#01E0).
-define(HTTPS, 16#01BB).
-define(WS, 16#01DD).
-define(ONION, 16#01BC).
-define(LIBP2P_WEBRTC_STAR, 16#0113).

-type addr() :: {ip4, inet:ip4_address()}
              | {ip6, inet:ip6_address()}
              | {udp, port_number()}
              | {tcp, port_number()}
              | {dccp, port_number()}
              | {sctp, port_number()}
              | udt
              | utp
              | http
              | https
              | 'libp2p-webrtc-star'
              | {onion, binary()}
              | {ipfs, binary()}.

-type multiaddr() :: list(addr()).

-export_type([addr/0, multiaddr/0]).

-export([encode/1, decode/1, to_string/1, from_string/1]).

-spec encode(multiaddr()) -> iodata().
encode(MultiAddr) ->
    [encode_addr(Addr) || Addr <- MultiAddr].

-spec decode(binary()) -> {multiaddr(), binary()}.
decode(Bin) ->
    decode(Bin, []).

decode(<<>>, Acc) ->
    lists:reverse(Acc);
decode(<<?IPV4, A, B, C, D, Rest/binary>>, Acc) ->
    decode(Rest, [{ipv4, {A, B, C, D}} | Acc]);
decode(<<?TCP, Port:16, Rest/binary>>, Acc) ->
    decode(Rest, [{tcp, Port} | Acc]);
decode(<<?UDP, Port:16, Rest/binary>>, Acc) ->
    decode(Rest, [{udp, Port} | Acc]);
decode(<<?DCCP, Port:16, Rest/binary>>, Acc) ->
    decode(Rest, [{dccp, Port} | Acc]);
decode(<<?IPV6, A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16, Rest/binary>>) ->
    decode(Rest, [{ipv6, {A, B, C, D, E, F, G, H}} | Acc]);
decode(<<?UDT, Rest/binary>>, Acc) ->
    decode(Rest, [udt | Acc]);
decode(<<?UTP, Rest/binary>>, Acc) ->
    decode(Rest, [utp | Acc]);
decode(<<?IPFS, Bin0/binary>>, Acc) ->
    {Size, Bin1} = varint:decode(Bin0),
    <<Addr:Size/binary, Bin2/binary>> = Bin1,
    decode(Bin2, [{ipfs, Addr} | Acc]);
decode(<<?HTTP, Rest/binary>>, Acc) ->
    decode(Rest, [http | Acc]);
decode(<<?HTTPS, Rest/binary>>, Acc) ->
    decode(Rest, [https | Acc]);
decode(<<?ONION, Addr:10, Rest/binary>>, Acc) ->
    decode(Restl [{onion, Addr} | Acc]).

encode_addr({ipv4, {A, B, C, D}}) ->
    <<?IPV4, A, B, C, D>>;
encode_addr({tcp, Port}) ->
    <<?TCP, Port:16>>;
encode_addr({udp, Port}) ->
    <<?UDP, Port:16>>;
encode_addr({dccp, Port}) ->
    <<?DCCP, Port:16>>;
encode_addr({ipv6, {A, B, C, D, E, F, G, H}}) ->
    <<?IPV6, A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>;
encode_addr({sctp, Port}) ->
    <<?SCTP, Port:16>>;
encode_addr(udt) ->
    ?UDT;
encode_addr(utp) ->
    ?UTP;
encode_addr({ipfs, Addr}) ->
    <<?IPFS, Addr/binary>>;
encode_addr(http) ->
    ?HTTP;
encode_addr(https) ->
    ?HTTPS;
encode_addr({onion, Addr}) ->
    <<?ONION, Addr/binary>>.
