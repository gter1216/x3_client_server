%% @author xxu
%% @copyright 2017 Nokia, Inc
%% This module used to constuct all X3 interface message.


-module(x3_lib).
-include("X3-PROTOCOL.hrl").
-include("x3_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([cons_create_lict_req/4, cons_x3_check_state_req/1]).


%% ==================================================================
%% input:
%%      NeId => OCTET STRING(SIZE(1..256))
%%              "sbc7510"
%%
%% ouput:
%%      Bytes Msg => Check State Req
%%
%% ==================================================================
cons_create_lict_req(MsgSerialNo, NeId, ICIDValue, CCCId) ->
	
	CreateLICTReq = #'CreateLICTReq'{messageSerialNo = MsgSerialNo,
									 neID = NeId,
									 icidValue = ICIDValue,
									 'cCC-ID' = CCCId},
	
	X3CmdMessage = {createLictReq, CreateLICTReq},
	
	X3InterfaceMsg = #'X3Interface'{protocolVersion = ?protocol_version,
									x3cmdMessage = X3CmdMessage},
	
    {ok, Bytes} = 'X3-PROTOCOL':encode('X3Interface', X3InterfaceMsg),

    add_x3_header(Bytes).

%% ==================================================================
%% input:
%%      NeId => OCTET STRING(SIZE(1..256))
%%              "sbc7510"
%%
%% ouput:
%%      Bytes Msg => Check State Req
%%
%% ==================================================================
cons_x3_check_state_req(NeId) ->
	
	X3CheckStateReq = #'X3CheckStateReq'{neID = NeId},
	
	X3CmdMessage = {x3CheckStateReq, X3CheckStateReq},
	
	X3InterfaceMsg = #'X3Interface'{protocolVersion = ?protocol_version,
									x3cmdMessage = X3CmdMessage},
	
    {ok, Bytes} = 'X3-PROTOCOL':encode('X3Interface', X3InterfaceMsg),

    Bytes.





%% ====================================================================
%% Internal functions
%% ====================================================================


%% ==================================================================
%% 
%% add x3 header for the input bytes
%%
%% x3 header format => 
%%       16#aa, 16#00, 2 bytes size, 2 bytes size
%% 
%%       16#aa: sycn bytes
%%       16#00: not ciphered
%%       message bytes size
%%
%% ==================================================================
add_x3_header(Bytes) ->
	
    Len = size(Bytes),

    <<16#aa, 16#00, Len:16, Len:16, Bytes/binary>>.
	







