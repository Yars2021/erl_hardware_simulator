-module(nn_proc).
-export([create/1,
         init/2]).


% Create NN model
create(PE_CORES_NUM) ->
    LocalMem = spawn_local_mem(PE_CORES_NUM, spawn_PE_cores(PE_CORES_NUM)),
    RAM = spawn(fun() -> ram:listen([]) end),
    Memory = spawn(fun() -> memory:listen([]) end),
    IO_Controller = spawn(fun() -> io_controller:listen(0, 0) end),
    BusMatrix = spawn(fun() -> bus_matrix:listen(IO_Controller, Memory, RAM, LocalMem) end),
    ControlUnit = spawn(fun() -> control_unit:listen() end),

    IO_Controller ! {register_bus, BusMatrix},
    IO_Controller ! {register_control_unit, ControlUnit},

    {IO_Controller, [ControlUnit | [BusMatrix | [IO_Controller | [Memory | [RAM | LocalMem]]]]]}.


% Start NN model
init(IO_Controller, [InputFile, WeightsFile]) ->
    IO_Controller ! {input, InputFile},
    sim:delay(10),
    IO_Controller ! {weights, WeightsFile},
    sim:delay(10),
    IO_Controller ! {read_RAM}.


spawn_PE_cores(0) -> [];
spawn_PE_cores(N) -> [spawn(fun() -> pe_core:listen() end) | spawn_PE_cores(N - 1)].


spawn_local_mem(0, _) -> [];
spawn_local_mem(N, [PE_Core | Tail]) ->
    [spawn(fun() -> local_memory:listen(PE_Core, [], [], [], [], []) end) | spawn_local_mem(N - 1, Tail)].