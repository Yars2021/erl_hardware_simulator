-module(bus_matrix).
-export([listen/3]).


listen(RAM, Memory, LocalMem) ->
    receive
        % RAM -> Bus -> LocalMem
        {calc_for_inputs, Inputs} ->
            lists:map(LocalMem, fun(LocalMemPID) -> LocalMemPID ! {write, inputs, Inputs} end);

        % Memory -> Bus -> LocalMem
        {calc_for_weights, Weights} ->
            lists:map(LocalMem, fun(LocalMemPID) -> LocalMemPID ! {write, weights, Weights} end);

        % RAM -> Bus -> IO_Controller
        {output_results, IO, Results} -> IO ! {output, Results};

        % IO_Controller -> Bus -> RAM
        {write_input, InputVector} -> RAM ! {write_vector, InputVector};

        % IO_Controller -> Bus -> Memory
        {write_weights, WeightsVector} -> Memory ! {write_weights, WeightsVector};

        % Memory -> Bus -> LocalMem
        {weight_matrix, WeightMatrix} -> distribute_matrix(LocalMem, WeightMatrix);

        % LocalMem -> Bus -> RAM
        {result, Index, Value} -> RAM ! {value, Index, Value};


        % For testing
        {read_RAM, IO} -> RAM ! {send_to_output, IO, self()};
        {read_Memory, IO} -> Memory ! {send_to_output, IO, self()};

        _ -> listen(RAM, Memory, LocalMem)
    end,

    listen(RAM, Memory, LocalMem).


distribute_matrix(LocalMemPIDs, WeightMatrix) -> 0.