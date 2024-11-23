-module(local_memory).
-export([listen/5]).


listen(IndexMemory, InputMemory, WeightMemory, VectorMulMemory, SigmMemory) ->
    receive
        {write, inputs, List} -> listen(IndexMemory, InputMemory ++ [List], WeightMemory, VectorMulMemory, SigmMemory);
        {write, weights, List} -> listen(IndexMemory, InputMemory, WeightMemory ++ [List], VectorMulMemory, SigmMemory);
        {write, vector_mul, Value} -> listen(IndexMemory, InputMemory, WeightMemory, VectorMulMemory ++ [Value], SigmMemory);
        {write, activation, Value} -> listen(IndexMemory, InputMemory, WeightMemory, VectorMulMemory, SigmMemory ++ [Value]);

        {PE, read, inputs_and_weights} ->
            [FirstInput | InputTail] = InputMemory,
            [FirstWeight | WeightTail] = WeightMemory,
            PE ! {self(), vector_mul, FirstInput, FirstWeight},
            listen(IndexMemory, InputTail, WeightTail, VectorMulMemory, SigmMemory);

        {PE, read, vector_mul} ->
            [FirstVectorMul | VectorMulTail] = VectorMulMemory,
            PE ! {self(), activation_func, FirstVectorMul},
            listen(IndexMemory, InputMemory, WeightMemory, VectorMulTail, SigmMemory);

        {Bus, get_result} ->
            [FirstSigm | SigmTail] = SigmMemory,
            [FirstIndex | IndexTail] = IndexMemory,
            Bus ! {result, FirstIndex, FirstSigm},
            listen(IndexTail, InputMemory, WeightMemory, VectorMulMemory, SigmTail);

        _ -> listen(IndexMemory, InputMemory, WeightMemory, VectorMulMemory, SigmMemory)
    end.