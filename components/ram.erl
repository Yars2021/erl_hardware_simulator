-module(ram).
-export([listen/1]).


listen(Memory) ->
    receive
        {write_vector, Vector} -> listen(write_to_ram(Vector));
        {send_to_calc, Bus} -> Bus ! {calc_for_inputs, flatten(Memory)};
        {send_to_output, IO, Bus} -> Bus ! {output_results, IO, Memory};
        {value, Index, Value} -> listen(lists:sort([{Index, Value} | Memory]));
        {erase} -> listen([]);
        _ -> listen(Memory)
    end,

    listen(Memory).


write_to_ram(List) -> write_to_ram(List, 1).

write_to_ram([], _) -> [];
write_to_ram([Head | Tail], Index) -> [{Index, Head} | write_to_ram(Tail, Index + 1)].


flatten([]) -> [];
flatten([{_, Value} | Tail]) -> [Value | flatten(Tail)].