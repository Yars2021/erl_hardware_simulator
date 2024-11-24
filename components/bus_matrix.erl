-module(bus_matrix).
-export([listen/4]).


listen(IO_Controller, Memory, RAM, LocalMem) ->
    receive
        % RAM/Memory -> Bus -> IO_Controller
        {output_results, Results} -> IO_Controller ! {output, Results};

        % Read RAM
        {read_RAM} -> RAM ! {send_to_output, self()};

        % Read Memory
        {read_Memory} -> Memory ! {send_to_output, self()};




        % RAM -> Bus -> LocalMem
        {calc_for_inputs, Inputs} ->
            lists:map(LocalMem, fun(LocalMemPID) -> LocalMemPID ! {write, inputs, Inputs} end);

        % Memory -> Bus -> LocalMem
        {calc_for_weights, Weights} -> distribute_matrix(LocalMem, Weights);

        % IO_Controller -> Bus -> RAM
        {write_input, InputVector} -> RAM ! {write_vector, InputVector};

        % IO_Controller -> Bus -> Memory
        {write_weights, WeightsVector} -> Memory ! {write_weights, WeightsVector};

        % LocalMem -> Bus -> RAM
        {result, Index, Value} -> RAM ! {value, Index, Value};

        _ -> listen(IO_Controller, Memory, RAM, LocalMem)
    end,

    listen(IO_Controller, Memory, RAM, LocalMem).


distribute_matrix(LocalMemPIDs, WeightMatrix) ->
    distribute_matrix(LocalMemPIDs, LocalMemPIDs, WeightMatrix, 1).

distribute_matrix(_, _, [], _) -> 0;

distribute_matrix(InitialPIDs, [], Matrix, Index) ->
    distribute_matrix(InitialPIDs, InitialPIDs, Matrix, Index);

distribute_matrix(InitialPIDs, [PID | PIDTail], [Vector | MatrixTail], Index) ->
    PID ! {write, index, Index},
    PID ! {write, weights, Vector},
    distribute_matrix(InitialPIDs, PIDTail, MatrixTail, Index + 1).