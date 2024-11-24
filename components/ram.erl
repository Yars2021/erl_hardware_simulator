-module(ram).
-export([listen/1]).


% ff_logic, clk driven
listen(Memory) ->
    receive
        % Erase RAM
        {erase} -> receive {clk} -> listen([]) end;

        % Write a vector
        {write_vector, Vector} -> receive {clk} -> listen(write_to_ram(Vector)) end;

        % Write one item
        {value, Index, Value} -> receive {clk} -> listen(lists:sort([{Index, Value} | Memory])) end;

        % Send RAM value to LocalMemory through Bus
        {send_to_calc, Bus} -> receive {clk} -> Bus ! {calc_for_inputs, flatten(Memory)} end;

        % Send RAM value to IO through Bus
        {send_to_output, Bus} -> receive {clk} -> Bus ! {output_results, Memory} end;

        _ -> listen(Memory)
    end,

    listen(Memory).


write_to_ram(List) -> write_to_ram(List, 1).

write_to_ram([], _) -> [];
write_to_ram([Head | Tail], Index) -> [{Index, Head} | write_to_ram(Tail, Index + 1)].


flatten([]) -> [];
flatten([{_, Value} | Tail]) -> [Value | flatten(Tail)].