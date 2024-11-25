-module(sim).
-export([start/4,
         return/1,
         delay/1]).


% Start any model
start(Model, SimulationTimeout, ModelType, Args) ->
    register(model, spawn(
        fun() ->
            {IO_Controller, Components} = Model,

            ModelStarter = spawn(fun() ->
                case ModelType of
                    nn_proc -> nn_proc:init(IO_Controller, Args);
                    _ -> 0 % Easy to scale
                end
            end),

            Clock = spawn(fun() -> clk_gen(0, ModelStarter, Components) end),

            receive
                % Terminate simulation
                {sim_end} -> print_clock(terminate, Clock);

                % Print output and finish simulation
                {sim_finish, Output} ->
                    delay(1),
                    io:format("================================~n"),
                    write_memory(Output),
                    print_clock(finish, Clock)

            after SimulationTimeout ->
                print_clock(timeout, Clock)
            end
        end)).


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


print_clock(Reason, Clock) ->
    Clock ! {stop, self()},
    receive
        {ticks, Counter} ->
            io:format("================================~n"),
            sim_end_msg(Reason, Counter)
    end.


sim_end_msg(terminate, Counter) -> io:format("Simulation terminated. Ticks: ~p.~n", [Counter]);
sim_end_msg(finish, Counter) -> io:format("Simulation finished in ~p ticks.~n", [Counter]);
sim_end_msg(timeout, Counter) -> io:format("Simulation timed out. Ticks: ~p.~n", [Counter]).


write_memory([]) -> io:format("None~n");
write_memory([{Address, Value}]) -> io:format("Address ~p -> ~p~n", [Address, Value]);
write_memory([{Address, Value} | Tail]) ->
    io:format("Address ~p -> ~p~n", [Address, Value]),
    write_memory(Tail).