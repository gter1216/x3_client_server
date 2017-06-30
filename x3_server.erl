%% @author xxu <xiao.a.xu@alcatel-sbell.com.cn>
%% @copyright 2017 Nokia, Inc
%% This module used to implement X3 server behaviour,
%% i.e. simulate as LIC server.

-module(x3_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, stop/1]).

-record(state, {pid_name}).


%% ====================================================================
%% start serverl TCP server(LIC Server) to monitor several 
%% port by PortList.
%%
%% input:  PortList => [Port1, Port2, ..., PortN] 
%%
%%
%% output: PidList = > [<0.79.0>,<0.78.0>,<0.77.0>]
%%
%% ====================================================================
start(PortList) ->
    
    lists:foldl(fun(Port, Acc) -> 
								  Pid = spawn(fun() -> start_server(Port) end),
								  PidName = list_to_atom(integer_to_list(Port)),
								  register(PidName, Pid),
								  [Pid|Acc]
						  end, [], PortList).


%% ====================================================================
%%
%% stop TCP server(LIC Server) by kill Pid in the PidList.
%%
%% ====================================================================
stop(PortList) ->
    
    lists:foreach(fun(Port) ->
						  PidName = list_to_atom(integer_to_list(Port)),
						  Pid =  whereis(PidName),
						  exit(Pid, kill)
				  end, PortList).

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
	
	par_connect(Listen, Port).


par_connect(Listen, ServerPort) ->
	
	{ok, Socket} = gen_tcp:accept(Listen),
	
	{ok, {ClientIP, ClientPort}} = inet:peername(Socket),
	
	PidName = make_pid_name(ServerPort, ClientPort),

	Pid = spawn(fun() -> loop(Socket, #state{pid_name = PidName}) end),
	
	register(PidName, Pid),
	
	io:format("Worker ~w (~w) start receive packets from ip: ~w, port: ~w ... ~n", 
			  [PidName, Pid, ClientIP, ClientPort]),
	
	par_connect(Listen, ServerPort).


loop(Socket, State = #state{pid_name = PidName}) ->
	
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
%%     		gen_tcp:send(Socket, Data),
            io:format("Worker ~w received data = ~p~n", [PidName, Bin]),
%% 			handle_data(Bin),
            loop(Socket, State);
		
		{error, closed} ->
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
	SName = atom_to_list(s) ++ integer_to_list(SPort),
	CName = atom_to_list(c) ++ integer_to_list(CPort),
	FullName = SName ++ "_" ++ CName,
    list_to_atom(FullName).
	
	








