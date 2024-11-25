-module(memory).
-export([listen/1]).


% ff_logic, clk driven
listen(Memory) ->
    receive
        % Write vector of weight matrices
        {write_weights, WeightsVector} -> receive {clk} -> listen(WeightsVector) end;

        % Send the length of the matrix vector to ControlUnit
        {get_num_of_layers, ControlUnit} -> receive {clk} -> ControlUnit ! {num_of_layers, length(Memory)} end;

        % Send weights matrix to LocalMemory through Bus
        {send_to_calc, CalcNum, Bus} -> receive {clk} -> Bus ! {calc_for_weights, lists:nth(Memory, length(Memory) - CalcNum) + 1} end;

        % Send weights to IO through Bus
        {send_to_output, Bus} -> receive {clk} -> Bus ! {output_results, format(Memory)} end;

        _ -> listen(Memory)
    end,

    listen(Memory).


format(List) -> format(List, 1).

format([], _) -> [];
format([Matrix | Tail], Addr) -> [{Addr, Matrix} | format(Tail, Addr + 1)].