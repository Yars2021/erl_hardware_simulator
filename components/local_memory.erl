-module(local_memory).
-export([listen/6]).


% ff_logic, clk driven
listen(PE, IndexMemory, InputMemory, WeightMemory, VectorMulMemory, SigmoidMemory) ->
    receive
        {clk} ->
            receive
                % Erase LocalMemory cell
                {erase} -> listen(PE, [], [], [], [], []);

                % Append a new index value
                {write, index, Index} -> listen(PE, IndexMemory ++ [Index], InputMemory, WeightMemory, VectorMulMemory, SigmoidMemory);

                % Append new inputs vector
                {write, inputs, List} -> listen(PE, IndexMemory, InputMemory ++ [List], WeightMemory, VectorMulMemory, SigmoidMemory);

                % Append new weights vector
                {write, weights, List} -> listen(PE, IndexMemory, InputMemory, WeightMemory ++ [List], VectorMulMemory, SigmoidMemory);

                % Append new vector_mul value
                {write, vector_mul, Value} -> listen(PE, IndexMemory, InputMemory, WeightMemory, VectorMulMemory ++ [Value], SigmoidMemory);

                % Append new activation_func value
                {write, activation, Value} -> listen(PE, IndexMemory, InputMemory, WeightMemory, VectorMulMemory, SigmoidMemory ++ [Value]);

                % Send inputs and weights vectors to mul PE. Pop weights
                {calc, inputs_and_weights} ->
                    [InputVector] = InputMemory,
                    [FirstWeight | WeightTail] = WeightMemory,
                    PE ! {self(), vector_mul, InputVector, FirstWeight},
                    listen(PE, IndexMemory, InputMemory, WeightTail, VectorMulMemory, SigmoidMemory);

                % Send vector_mul to PE. Pop vector_mul
                {calc, vector_mul} ->
                    [FirstVectorMul | VectorMulTail] = VectorMulMemory,
                    PE ! {self(), activation_func, FirstVectorMul},
                    listen(PE, IndexMemory, InputMemory, WeightMemory, VectorMulTail, SigmoidMemory);

                % Send results to Bus. Pop sigmoid and index
                {get_result, Bus} ->
                    [FirstIndex | IndexTail] = IndexMemory,
                    [FirstSigmoid | SigmoidTail] = SigmoidMemory,
                    Bus ! {result, FirstIndex, FirstSigmoid},
                    listen(PE, IndexTail, InputMemory, WeightMemory, VectorMulMemory, SigmoidTail);

                _ -> listen(PE, IndexMemory, InputMemory, WeightMemory, VectorMulMemory, SigmoidMemory)
            end;

        _ -> listen(PE, IndexMemory, InputMemory, WeightMemory, VectorMulMemory, SigmoidMemory)
    end,

    listen(PE, IndexMemory, InputMemory, WeightMemory, VectorMulMemory, SigmoidMemory).