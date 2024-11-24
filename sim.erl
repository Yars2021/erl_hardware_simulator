-module(sim).
-export([start/4,
         return/1,
         delay/1]).


% Start any model
start(Model, SimulationTimeout, ModelType, Args) ->
    register(model, spawn(fun() -> run_model(Model, SimulationTimeout, ModelType, Args) end)).

run_model(Model, SimulationTimeout, ModelType, Args) ->
    {IO_Controller, Components} = Model,

    ModelStarter = spawn(fun() -> init_model(ModelType, IO_Controller, Args) end),
    Clock = spawn(fun() -> clk_gen(0, ModelStarter, Components) end),

    receive
        % Terminate simulation
        {sim_end} -> print_clock(Clock);

        % Print output and finish simulation
        {sim_finish, Output} ->
            delay(1),
            io:format("================================~n"),
            write_memory(Output),
            print_clock(Clock)

    after SimulationTimeout ->
        print_clock(Clock)
    end.

init_model(ModelType, IO_Controller, Args) ->
    case ModelType of
        nn_proc -> nn_proc:init(IO_Controller, Args)
    end.


% API for models
return(Data) -> {model, node()} ! {sim_finish, Data}.
delay(MS) -> receive after MS -> 0 end.


clk_gen(Counter, Starter, Model) ->
    receive
        {stop, PID} -> PID ! {ticks, Counter}
    after 5 ->
        Starter ! {clk},
        tick_all(Model),
        clk_gen(Counter + 1, Starter, Model)
    end.


tick_all([]) -> 0;
tick_all([Node | NodesTail]) -> Node ! {clk}, tick_all(NodesTail).


print_clock(Clock) ->
    Clock ! {stop, self()},
    receive
        {ticks, Counter} -> io:format("================================~nSimulation ended in ~p ticks.~n", [Counter])
    end.


write_memory([]) -> io:format("None~n");
write_memory([{Address, Value}]) -> io:format("Address ~p -> ~p~n", [Address, Value]);
write_memory([{Address, Value} | Tail]) ->
    io:format("Address ~p -> ~p~n", [Address, Value]),
    write_memory(Tail).