-module(pe_core).
-export([listen/0]).


listen() ->
    receive
        {Requester, vector_mul, Inputs, Weights} -> Requester ! {write, vector_mul, vector_mul(Inputs, Weights)};
        {Requester, activation_func, Value} -> Requester ! {write, activation, sigmoid(Value)};
        _ -> listen()
    end,

    listen().


sigmoid(X) -> 1 / (1 + math:exp(-X)).


vector_mul(_, []) -> 0;
vector_mul([], _) -> 0;
vector_mul([HeadA | TailA], [HeadB | TailB]) ->
	HeadA * HeadB + vector_mul(TailA, TailB).
