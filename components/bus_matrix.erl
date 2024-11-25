-module(bus_matrix).
-export([listen/4]).


% comb_logic
listen(IO_Controller, Memory, RAM, LocalMem) ->
    receive
        % RAM/Memory -> Bus -> IO_Controller
        {output_results, Results} -> IO_Controller ! {output, Results};

        % Read RAM
        {read_RAM} -> RAM ! {send_to_output, self()};

        % Read Memory
        {read_Memory} -> Memory ! {send_to_output, self()};

        % IO_Controller -> Bus -> RAM
        {write_input, InputVector} -> RAM ! {write_vector, InputVector};

        % IO_Controller -> Bus -> Memory
        {write_weights, WeightsVector} -> Memory ! {write_weights, WeightsVector};

        % RAM -> Bus -> LocalMem
        {calc_for_inputs, Inputs} -> lists:map(fun(LocalMemPID) -> LocalMemPID ! {write, inputs, Inputs} end, LocalMem);

        % split(RAM) -> Bus -> LocalMem
        {calc_for_inputs_distribute, Inputs} -> distribute_input(LocalMem, Inputs);

        % Memory -> Bus -> LocalMem
        {calc_for_weights, Weights} -> distribute_weights(LocalMem, Weights);

        % LocalMem -> Bus -> RAM
        {result, Index, Value} -> RAM ! {value, Index, Value};

        _ -> listen(IO_Controller, Memory, RAM, LocalMem)
    end,

    listen(IO_Controller, Memory, RAM, LocalMem).


distribute_input(LocalMemPIDs, InputVector) ->
    distribute_input(LocalMemPIDs, LocalMemPIDs, InputVector, 1).

distribute_input(_, _, [], _) -> 0;

distribute_input(InitialPIDs, [], InputVector, _) ->
    distribute_input(InitialPIDs, InitialPIDs, InputVector, 1);

distribute_input(InitialPIDs, [PID | PIDTail], [Input | InputTail], Index) ->
    PID ! {write, index, Index},
    PID ! {write, vector_mul, Input},
    distribute_input(InitialPIDs, PIDTail, InputTail, Index + 1).


distribute_weights(LocalMemPIDs, WeightMatrix) ->
    distribute_weights(LocalMemPIDs, LocalMemPIDs, WeightMatrix, 1).

distribute_weights(_, _, [], _) -> 0;

distribute_weights(InitialPIDs, [], Matrix, _) ->
    distribute_weights(InitialPIDs, InitialPIDs, Matrix, 1);

distribute_weights(InitialPIDs, [PID | PIDTail], [Vector | MatrixTail], Index) ->
    PID ! {write, index, Index},
    PID ! {write, weights, Vector},
    distribute_weights(InitialPIDs, PIDTail, MatrixTail, Index + 1).