-module(model).
-export([config/1,
         start/1,
         calculate_input/3,
         calculate_hidden/3]).


delay(MS) -> receive after MS -> 0 end.


config(PE_CORES_NUM) ->
    PE_Cores = spawn_PE_cores(PE_CORES_NUM),
    LocalMem = spawn_local_mem(PE_CORES_NUM),
    Memory = spawn(fun() -> memory:listen([]) end) ,
    RAM = spawn(fun() -> ram:listen([]) end),
    BusMatrix = spawn(fun() -> bus_matrix:listen(RAM, Memory, LocalMem) end),
    ControlUnit = 0,

    [ControlUnit, RAM, Memory, BusMatrix, LocalMem, PE_Cores].


start([ControlUnit, RAM, Memory, BusMatrix, LocalMem, PE_Cores]) ->
    register(nn_proc, spawn(fun() -> io_controller:listen(BusMatrix) end)).


spawn_PE_cores(0) -> [];
spawn_PE_cores(N) -> [spawn(fun() -> pe_core:listen() end) | spawn_PE_cores(N - 1)].


spawn_local_mem(0) -> [];
spawn_local_mem(N) -> [spawn(fun() -> local_memory:listen([], [], [], [], []) end) | spawn_local_mem(N - 1)].


% Вычисление для входного нейрона (просто функция активации)
calculate_input(Bus, LocalMem, PE) ->
    LocalMem ! {PE, read, vector_mul}, % LocalMem[vector_mul] -> PE -> LocalMem[activation]
    delay(5),
    LocalMem ! {Bus, get_result}. % LocalMem[activation] -> Bus


% Вычисление для скрытого или выходного нейрона (скалярное произведение входов и весов и функция активации)
calculate_hidden(Bus, LocalMem, PE) ->
    LocalMem ! {PE, read, inputs_and_weights}, % LocalMem[inputs, weights] -> PE -> LocalMem[vector_mul]
    delay(5),
    LocalMem ! {PE, read, vector_mul}, % LocalMem[vector_mul] -> PE -> LocalMem[activation]
    delay(5),
    LocalMem ! {Bus, get_result}. % LocalMem[activation] -> Bus