%% @author xxu
%% @copyright 2017 Nokia, Inc
%% This module used to implement X3 client behaviour,
%% i.e. simulate as SBC 7510.


-module(x3_client).
-include("X3-PROTOCOL.hrl").
-include("x3_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([test_case_001/0, test_case_001/4]).


%% ==================================================================
%% test_case_001
%% 
%% send create_lict_req message from SBC to LIC server.
%%
%%       client                  server
%%              create lict req
%%         ----------------------->
%%
%% ==================================================================
test_case_001() ->
	
	%% assign default value
	MsgSerialNo = 213,
	NeId = "PGW168",
    ICIDValue = "cscf-20170627152245",
    CCCId = 10156,
	
	test_case_001(MsgSerialNo, NeId, ICIDValue, CCCId).

test_case_001(MsgSerialNo, NeId, ICIDValue, CCCId) ->
	
	Bytes = x3_lib:cons_create_lict_req(MsgSerialNo, NeId, ICIDValue, CCCId),
	
	io:format("msg byte is ~w~n", [Bytes]),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
								   ?x3_server_port, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes).
	
	%% close the socket
%% 	ok = gen_tcp:close(Socket).


%% ====================================================================
%% Internal functions
%% ====================================================================


%% %% ==================================================================
%% %% 
%% %% send_message:
%% %%
%% %% client send message to LIC server by TCP socket.
%% %%
%% %% ==================================================================
%% send_message(Packet) ->
%% 	
%%     %% connect to LIC server and return the socket.
%% 
%%     {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
%% 								   ?x3_server_port, 
%% 								   [binary, {packet, 0}]),
%% 
%%     %% send message by the socket.
%% 
%%     ok = gen_tcp:send(Socket, Packet)













