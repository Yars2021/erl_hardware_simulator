-module(io_controller).
-export([listen/1]).


delay(MS) -> receive after MS -> 0 end.


listen(Bus) ->
    receive
        {output, Data} -> delay(5), write_output(Data);
        {input, Filename} -> Bus ! {write_input, read_term(Filename)};
        {weights, Filename} -> Bus ! {write_weights, read_term(Filename)};

        % For testing
        {read_RAM} -> Bus ! {read_RAM, self()};
        {read_Memory} -> Bus ! {read_Memory, self()}
    end,

    listen(Bus).


read_term(FileName) ->
    Lines = read_lines(FileName) ++ ".",
    {ok, Tokens, _} = erl_scan:string(Lines),
    {ok, Abstract} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Abstract, erl_eval:new_bindings()),
    Value.


write_output([]) -> io:format("None~n");
write_output([{Address, Value}]) -> io:format("Address: ~p -> ~p~n", [Address, Value]);
write_output([{Address, Value} | Tail]) ->
    io:format("Address: ~p -> ~p~n", [Address, Value]),
    write_output(Tail).


read_lines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
        after file:close(Device)
    end.


get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.