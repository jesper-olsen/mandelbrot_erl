-module(complex).

-include_lib("eunit/include/eunit.hrl").

-export([norm/1, norm_sqr/1, add/2, conjugate/1, divide/2, equal/2, exp/1, imaginary/1,
         mul/2, new/2, real/1, sub/2]).

norm(Z) ->
    math:sqrt(real(Z) * real(Z) + imaginary(Z) * imaginary(Z)).

norm_sqr(Z) ->
    real(Z) * real(Z) + imaginary(Z) * imaginary(Z).

add(Z1, Z2) ->
    new(real(Z1) + real(Z2), imaginary(Z1) + imaginary(Z2)).

conjugate(Z) ->
    new(real(Z), -imaginary(Z)).

divide(Z1, Z2) ->
    new((real(Z1) * real(Z2) + imaginary(Z1) * imaginary(Z2))
        / (real(Z2) * real(Z2) + imaginary(Z2) * imaginary(Z2)),
        (imaginary(Z1) * real(Z2) - real(Z1) * imaginary(Z2))
        / (real(Z2) * real(Z2) + imaginary(Z2) * imaginary(Z2))).

equal(Z1, Z2) ->
    erlang:abs(real(Z1) - real(Z2)) < 0.005
    andalso erlang:abs(imaginary(Z1) - imaginary(Z2)) < 0.005.

exp(Z) ->
    new(math:exp(real(Z)) * math:cos(imaginary(Z)), math:sin(imaginary(Z))).

imaginary({_, I}) ->
    I.

mul(Z1, Z2) ->
    new(real(Z1) * real(Z2) - imaginary(Z1) * imaginary(Z2),
        imaginary(Z1) * real(Z2) + real(Z1) * imaginary(Z2)).

new(R, I) ->
    {R, I}.

real({R, _}) ->
    R.

sub(Z1, Z2) ->
    new(real(Z1) - real(Z2), imaginary(Z1) - imaginary(Z2)).

norm_test() ->
    ?assert(norm({1, 1}) =:= math:sqrt(2)).

norm_sqr_test() ->
    ?assert(norm_sqr({1, 1}) =:= 2).

sqr_test() ->
    ?assert({-1, 0} =:= mul({0, 1}, {0, 1})).

mul_test() ->
    ?assert(mul({0, -6}, {6, -2}) =:= {-12, -36}).
