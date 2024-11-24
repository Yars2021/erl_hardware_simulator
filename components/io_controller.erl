-module(io_controller).
-export([listen/2]).


% ff_logic, clk driven
listen(Bus, ControlUnit) ->
    receive
        % For model configuration, these commands don't need clk
        {register_bus, BusPID} -> listen(BusPID, ControlUnit);
        {register_control_unit, ControlUnitPID} -> listen(Bus, ControlUnitPID);

        % Output data on clk
        {output, Data} -> receive {clk} -> sim:return(Data) end;

        % Input input_vector from file on clk
        {input, Filename} -> receive {clk} -> Bus ! {write_input, read_term(Filename)} end;

        % Input weight_matrices from file on clk
        {weights, Filename} -> receive {clk} -> Bus ! {write_weights, read_term(Filename)} end;

        % Read RAM value on clk
        {read_RAM} -> receive {clk} -> Bus ! {read_RAM} end;

        % Read Memory value on clk
        {read_Memory} -> receive {clk} -> Bus ! {read_Memory} end;

        _ -> listen(Bus, ControlUnit)
    end,

    listen(Bus, ControlUnit).


read_term(FileName) ->
    Lines = read_lines(FileName) ++ ".",
    {ok, Tokens, _} = erl_scan:string(Lines),
    {ok, Abstract} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Abstract, erl_eval:new_bindings()),
    Value.


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