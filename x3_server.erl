%% @author xxu <xiao.a.xu@alcatel-sbell.com.cn>
%% @copyright 2017 Nokia, Inc
%% This module used to implement X3 server behaviour,
%% i.e. simulate as LIC server.

-module(x3_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start_server/1, par_connect/2]).

%% -record(state, {}).


%% ====================================================================
%% start serverl TCP server(LIC Server) to monitor several 
%% port by PortList.
%%
%% input:  PortList => [Port1, Port2, ..., PortN] 
%%
%%
%% ====================================================================
start(PortList) ->
    
	Fun = fun(Port) ->
				  spawn(?MODULE, start_server, [Port])		  
		  end,

	lists:foreach(Fun, PortList).


%% ====================================================================
%% Internal functions
%% ====================================================================


%% ====================================================================
%% start_server()
%%
%% For each ClientIP+ClientPort pair, start a new worker to handle the 
%% connect, and loop receive the packet form this connection.
%%
%% input: Port => 50000 (integer)
%%
%%
%% ====================================================================
start_server(Port) ->
	
	{ok, Listen} = gen_tcp:listen(Port, [binary,
										 {active, false}]),
	
	io:format("Server parent worker ~w start to listen the port ~w ... ~n", [self(), Port]),
	
	spawn(?MODULE, par_connect, [Listen, Port]).


par_connect(Listen, ServerPort) ->
	
	{ok, Socket} = gen_tcp:accept(Listen),
	
	{ok, {ClientIP, ClientPort}} = inet:peername(Socket),
	
	PidName = make_pid_name(ServerPort, ClientPort),
	
	register(self(), PidName),
	
	io:format("Server child worker ~w start receive packets from client ip: ~w, port: ~w ... ~n", 
			  [PidName, ClientIP, ClientPort]),
	
%% 	%% This way is better, because we do not need export par_connect function.
%%  %% but not improve the old code, reason: lazy~~
%% 	spawn(fun() -> par_connect(Listen) end),

	spawn(?MODULE, par_connect, Listen),
	
	loop(Socket).


loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
            io:format("Server received data = ~p~n", [Bin]),
%% 			handle_data(Bin),
            loop(Socket);
		
		{tcp_closed, Socket} ->
			io:format("Server socket closed~n")
	end.


%% ====================================================================
%% make_pid_name()
%%
%% input: Port => 50000 (integer)
%%
%% output: server5000 (atom)
%% ====================================================================
make_pid_name(SPort,CPort) ->
	SName = atom_to_list(server) ++ integer_to_list(SPort),
	CName = atom_to_list(client) ++ integer_to_list(CPort),
	FullName = SName ++ "_" ++ CName,
    list_to_atom(FullName).
	
	








