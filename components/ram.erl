-module(ram).
-export([listen/1]).


% ff_logic, clk driven
listen(Memory) ->
    receive
        {clk} ->
            receive
                % Erase RAM
                {erase} -> listen([]);

                % Write a vector
                {write_vector, Vector} -> listen(write_to_ram(Vector));

                % Write one item
                {value, Index, Value} -> listen(lists:sort([{Index, Value} | Memory]));

                % Send RAM value to LocalMemory through Bus
                {send_to_calc, Bus} -> Bus ! {calc_for_inputs, flatten(Memory)};

                % Send RAM value to IO through Bus
                {send_to_output, Bus} -> Bus ! {output_results, Memory};

                _ -> listen(Memory)
            end;

        _ -> listen(Memory)
    end,

    listen(Memory).


write_to_ram(List) -> write_to_ram(List, 1).

write_to_ram([], _) -> [];
write_to_ram([Head | Tail], Index) -> [{Index, Head} | write_to_ram(Tail, Index + 1)].


flatten([]) -> [];
flatten([{_, Value} | Tail]) -> [Value | flatten(Tail)].