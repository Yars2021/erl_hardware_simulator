-module(memory).
-export([listen/1]).


listen(Memory) ->
    receive
        {write_weights, WeightsVector} -> listen(WeightsVector);
        {send_to_output, IO, Bus} -> Bus ! {output_results, IO, format(Memory)};
        _ -> listen(Memory)
    end,

    listen(Memory).


format(List) -> format(List, 1).

format([], _) -> [];
format([Matrix | Tail], Addr) -> [{Addr, Matrix} | format(Tail, Addr + 1)].