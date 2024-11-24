-module(memory).
-export([listen/1]).


% ff_logic, clk driven
listen(Memory) ->
    receive
        {clk} ->
            receive
                % Write vector of weight matrices
                {write_weights, WeightsVector} -> listen(WeightsVector);

                % Send the length of the matrix vector to ControlUnit
                {get_num_of_layers, ControlUnit} -> ControlUnit ! {num_of_layers, length(Memory)};

                % Send weights matrix to LocalMemory through Bus
                {send_to_calc, CalcNum, Bus} -> Bus ! {calc_for_weights, lists:nth(Memory, length(Memory) - CalcNum)};

                % Send weights to IO through Bus
                {send_to_output, Bus} -> Bus ! {output_results, format(Memory)};

                _ -> listen(Memory)
            end;

        _ -> listen(Memory)
    end,

    listen(Memory).


format(List) -> format(List, 1).

format([], _) -> [];
format([Matrix | Tail], Addr) -> [{Addr, Matrix} | format(Tail, Addr + 1)].