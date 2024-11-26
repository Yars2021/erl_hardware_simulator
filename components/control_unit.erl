-module(control_unit).
-export([execute/4]).


% Orchestrating forward propagation
execute(LocalMem, Bus, RAM, Memory) ->
    receive
        {start_calc} ->
            receive {clk} -> Memory ! {get_num_of_layers, self()} end, % Ask for layer count
            receive {clk} -> receive {num_of_layers, Layers} -> RAM ! {get_layer_len, self()} end end, % Ask for layer size
            receive {clk} -> receive {layer_len, LayerLen} ->
                input_cycle(LocalMem, Bus, RAM, LayerLen),
                layer_cycle(LocalMem, Bus, RAM, Memory, Layers)
            end end
    end.


% Input layer calc cycle
input_cycle(LocalMem, Bus, RAM, LayerSize) ->
    Iterations = round(math:ceil(LayerSize / length(LocalMem))),

    broadcast(LocalMem, {erase}), % Clear local memory
    receive {clk} -> RAM ! {send_to_calc_distribute, Bus} end, % Send layer inputs to cores
    receive {clk} -> RAM ! {erase} end, % Clear RAM
    receive {clk} -> receive {clk} -> looped_broadcast(Iterations, LocalMem, {calc, vector_mul}) end end, % Calculate activations
    receive {clk} -> sequential_looped_broadcast(Iterations, LocalMem, {get_result, Bus}) end, % Collect calculated input layer in RAM
    receive {clk} -> RAM ! {send_to_output, Bus} end. % Debug RAM output


% Full layer calc cycle
layer_cycle(LocalMem, Bus, RAM, Memory, Layers) ->
    % Get current layer size and do calc cycles until Layers is 0
    0.


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