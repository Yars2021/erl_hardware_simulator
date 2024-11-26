-module(control_unit).
-export([execute/4]).


% Orchestrating forward propagation
execute(LocalMem, Bus, RAM, Memory) ->
    receive
        {start_calc} ->
            receive {clk} -> Memory ! {get_num_of_layers, self()} end, % Ask for layer count
            receive {clk} -> receive {num_of_layers, Layers} ->
                receive {clk} ->
                    layer_cycle(LocalMem, Bus, RAM, Memory, Layers),
                    receive {clk} -> RAM ! {send_to_output, Bus} end % Send last layer to IO
                end
            end end
    end.


% Full layer calc cycle
layer_cycle(_, _, _, _, 0) -> 0;

layer_cycle(LocalMem, Bus, RAM, Memory, CalcNum) ->
    % Get current layer size and do calc cycles until CalcNum is 0
    receive {clk} -> RAM ! {get_layer_len, self()} end,
    receive {clk} -> receive {layer_len, LayerLen} ->
        Iterations = round(math:ceil(LayerLen / length(LocalMem))),

        broadcast(LocalMem, {erase}), % Clear local memory.
        receive {clk} -> RAM ! {send_to_calc, Bus} end, % Send layer inputs to cores
        receive {clk} -> Memory ! {send_weights_to_calc, CalcNum, Bus} end, % Send weights to cores
        receive {clk} -> RAM ! {erase} end, % Clear RAM
        receive {clk} -> receive {clk} -> looped_broadcast(Iterations, LocalMem, {calc, inputs_and_weights}) end end, % Calculate weighted input sum
        receive {clk} -> receive {clk} -> looped_broadcast(Iterations, LocalMem, {calc, vector_mul}) end end, % Calculate activations
        receive {clk} -> sequential_looped_broadcast(Iterations, LocalMem, {get_result, Bus}) end % Collect calculated layer in RAM
    end end,

    receive {clk} -> layer_cycle(LocalMem, Bus, RAM, Memory, CalcNum - 1) end.


looped_broadcast(0, _, _) -> 0;
looped_broadcast(Times, PIDs, Message) ->
    receive {clk} -> broadcast(PIDs, Message) end,
    receive {clk} -> looped_broadcast(Times - 1, PIDs, Message) end.


broadcast([], _) -> 0;
broadcast([PID | PID_Tail], Message) ->
    PID ! Message,
    broadcast(PID_Tail, Message).


sequential_looped_broadcast(0, _, _) -> 0;
sequential_looped_broadcast(Times, PIDs, Message) ->
    receive {clk} -> sequential_broadcast(PIDs, Message) end,
    receive {clk} -> sequential_looped_broadcast(Times - 1, PIDs, Message) end.


sequential_broadcast([], _) -> 0;
sequential_broadcast([PID | PID_Tail], Message) ->
    receive
        {clk} ->
            PID ! Message,
            sequential_broadcast(PID_Tail, Message)
    end.