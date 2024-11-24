-module(control_unit).
-export([listen/0]).


listen() ->
    receive after 10 -> listen() end.


% calculate_layer(PE_Cores, LocalMem, Bus, Memory, RAM, CalculationNum) ->
%     erase_local_mem(LocalMem),
%     RAM ! {send_to_calc, Bus},
%     Memory ! {send_to_calc, CalculationNum, Bus},
%     RAM ! {erase},
%     calculate_neurons(PE_Cores, LocalMem).
%
%
% erase_local_mem([]) -> 0;
%
% erase_local_mem([PID | PID_Tail]) ->
%     PID ! {erase},
%     erase_local_mem(PID_Tail).