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

-type addr_part() :: {ip4, inet:ip4_address()}
                   | {ip6, inet:ip6_address()}
                   | {udp, inet:port_number()}
                   | {tcp, inet:port_number()}
                   | {dccp, inet:port_number()}
                   | {sctp, inet:port_number()}
                   | udt
                   | utp
                   | {ipfs, binary()}
                   | http
                   | https
                   | ws
                   | {onion, binary()}
                   | 'libp2p-webrtc-star'.

-type multiaddr() :: list(addr_part()).

-export_type([addr_part/0, multiaddr/0]).

-export([encode/1, decode/1, to_string/1, from_string/1]).

-spec encode(multiaddr()) -> iodata().
encode(MultiAddr) ->
    [begin
         case encode_addr(Addr) of
             {Code, Data} -> [varint:encode(Code), Data];
             Code -> varint:encode(Code)
         end
     end
     || Addr <- MultiAddr].

-spec decode(binary()) -> multiaddr().
decode(Bin) ->
    decode(Bin, []).

-spec to_string(multiaddr()) -> unicode:chardata().
to_string(Parts) ->
    [part_to_str(Part) || Part <- Parts].

-spec from_string(unicode:chardata()) -> multiaddr().
from_string(Bin) ->
    from_string(unicode:characters_to_binary(Bin), []).

decode(<<>>, Acc) ->
    lists:reverse(Acc);
decode(Bin0, Acc) ->
    {Code, Bin1} = varint:decode(Bin0),
    {Part, Bin2} = do_decode(Code, Bin1),
    decode(Bin2, [Part | Acc]).

do_decode(?IP4, <<A, B, C, D, Rest/binary>>) ->
    {{ip4, {A, B, C, D}}, Rest};
do_decode(?IP6, <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16, Rest/binary>>) ->
    {{ip6, {A, B, C, D, E, F, G, H}}, Rest};
do_decode(?TCP, <<Port:16, Rest/binary>>) ->
    {{tcp, Port}, Rest};
do_decode(?UDP, <<Port:16, Rest/binary>>) ->
    {{udp, Port}, Rest};
do_decode(?DCCP, <<Port:16, Rest/binary>>) ->
    {{dccp, Port}, Rest};
do_decode(?SCTP, <<Port:16, Rest/binary>>) ->
    {{sctp, Port}, Rest};
do_decode(?UDT, Rest) ->
    {udt, Rest};
do_decode(?UTP, Rest) ->
    {utp, Rest};
do_decode(?IPFS, Bin0) ->
    {Size, Bin1} = varint:decode(Bin0),
    <<Addr:Size/binary, Bin2/binary>> = Bin1,
    {{ipfs, Addr}, Bin2};
do_decode(?HTTP, Rest) ->
    {http, Rest};
do_decode(?HTTPS, Rest) ->
    {https, Rest};
do_decode(?WS, Rest) ->
    {ws, Rest};
do_decode(?ONION, <<Addr:10, Rest/binary>>) ->
    {{onion, Addr}, Rest};
do_decode(?LIBP2P_WEBRTC_STAR, Rest) ->
    {'libp2p-webrtc-star', Rest}.

encode_addr({ip4, {A, B, C, D}}) ->
    {?IP4, <<A, B, C, D>>};
encode_addr({ip6, {A, B, C, D, E, F, G, H}}) ->
    {?IP6, <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>};
encode_addr({tcp, Port}) ->
    {?TCP, <<Port:16>>};
encode_addr({udp, Port}) ->
    {?UDP, <<Port:16>>};
encode_addr({dccp, Port}) ->
    {?DCCP, <<Port:16>>};
encode_addr({sctp, Port}) ->
    {?SCTP, <<Port:16>>};
encode_addr(udt) ->
    ?UDT;
encode_addr(utp) ->
    ?UTP;
encode_addr({ipfs, Addr}) ->
    {?IPFS, [varint:encode(byte_size(Addr)), Addr]};
encode_addr(https) ->
    ?HTTPS;
encode_addr(http) ->
    ?HTTP;
encode_addr(ws) ->
    ?WS;
encode_addr({onion, Addr}) ->
    {?ONION, <<Addr:10>>};
encode_addr('libp2p-webrtc-star') ->
    ?LIBP2P_WEBRTC_STAR.

part_to_str({ip4, IP4}) ->
    [BA, BB, BC, BD] = [integer_to_binary(X) || X <- tuple_to_list(IP4)],
    [<<"/ip4/">>, BA, $., BB, $., BC, $., BD];
part_to_str({ip6, IP6}) ->
    %% Compress IPv6 addresses
    {LIP60A, LIP60B} = lists:splitwith(fun (X) -> X /= 0 end, tuple_to_list(IP6)),
    LIP61 = lists:dropwhile(fun (X) -> X == 0 end, LIP60B),
    Start = [integer_to_binary(X, 16) || X <- LIP60A],
    End = [integer_to_binary(X, 16) || X <- LIP61],
    [<<"/ip6/">>, lists:join($:, Start), <<"::">>, lists:join($:, End)];
part_to_str({tcp, Port}) ->
    [<<"/tcp/">>, integer_to_binary(Port)];
part_to_str({udp, Port}) ->
    [<<"/udp/">>, integer_to_binary(Port)];
part_to_str({dccp, Port}) ->
    [<<"/dccp/">>, integer_to_binary(Port)];
part_to_str({sctp, Port}) ->
    [<<"/sctp/">>, integer_to_binary(Port)];
part_to_str(udt) ->
    <<"/udt">>;
part_to_str(utp) ->
    <<"/utp">>;
part_to_str({ipfs, Addr}) ->
    [<<"/ipfs/">>, multibase:encode(base64, Addr)];
part_to_str(https) ->
    <<"/https">>;
part_to_str(http) ->
    <<"/http">>;
part_to_str(ws) ->
    <<"/ws">>;
part_to_str('libp2p-webrtc-star') ->
    <<"/libp2p-webrtc-star">>.

from_string(<<>>, Acc) ->
    lists:reverse(Acc);
from_string(Bin0, Acc) ->
    {Part, Bin1} = from_str(Bin0),
    from_string(Bin1, [Part | Acc]).

from_str(<<"/ip4/", Bin0/binary>>) ->
    {BIP4, Bin1} = split_slash(Bin0),
    {ok, IP4} = inet:parse_ipv4strict_address(binary_to_list(BIP4)),
    {{ip4, IP4}, Bin1};
from_str(<<"/ip6/", Bin0/binary>>) ->
    {BIP6, Bin1} = split_slash(Bin0),
    {ok, IP6} = inet:parse_ipv6strict_address(binary_to_list(BIP6)),
    {{ip6, IP6}, Bin1};
from_str(<<"/tcp/", Bin0/binary>>) ->
    {BPort, Bin1} = split_slash(Bin0),
    {{tcp, binary_to_integer(BPort)}, Bin1};
from_str(<<"/udp/", Bin0/binary>>) ->
    {BPort, Bin1} = split_slash(Bin0),
    {{udp, binary_to_integer(BPort)}, Bin1};
from_str(<<"/dccp/", Bin0/binary>>) ->
    {BPort, Bin1} = split_slash(Bin0),
    {{dccp, binary_to_integer(BPort)}, Bin1};
from_str(<<"/sctp/", Bin0/binary>>) ->
    {BPort, Bin1} = split_slash(Bin0),
    {{sctp, binary_to_integer(BPort)}, Bin1};
from_str(<<"udt", Rest/binary>>) ->
    {udt, Rest};
from_str(<<"/utp", Rest/binary>>) ->
    {utp, Rest};
from_str(<<"/ipfs/", Bin0/binary>>) ->
    [MultiBaseAddr, Bin1] = binary:split(Bin0, <<$/>>),
    Addr = multibase:decode(MultiBaseAddr),
    true = is_binary(Addr),
    {{ipfs, Addr}, Bin1};
from_str(<<"/https", Rest/binary>>) ->
    {https, Rest};
from_str(<<"/http", Rest/binary>>) ->
    {http, Rest};
from_str(<<"/ws", Rest/binary>>) ->
    {ws, Rest};
from_str(<<"/libp2p-webrtc-star", Rest/binary>>) ->
    {'libp2p-webrtc-star', Rest}.

split_slash(Bin) ->
    {S, _} = binary:match(Bin, <<$/>>),
    {binary_part(Bin, 0, S), binary_part(Bin, S, byte_size(Bin) - S)}.
