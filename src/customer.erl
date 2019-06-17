-module(customer).
-author("Wasim Alayoubi").
%% API
-export([run/5]).

run(stop, ServerPId, Name, RequestedLoan, CurrentCollected)->
  ServerPId ! {messageRO, Name, RequestedLoan},
  ServerPId ! {unregisterCustomer, Name},
  CurrentCollected,
  done;
run(new, ServerPId, Name, RequestedLoan, CurrentCollected)->
  ServerPId ! {registerCustomer, Name, self()},
  run(continue, ServerPId, Name, RequestedLoan, CurrentCollected);
run(continue, ServerPId, Name, RequestedLoan, CurrentCollected) ->
  timer:sleep(rand:uniform(3000)),
  RemainingAmount = RequestedLoan - CurrentCollected ,
  if
    RemainingAmount < 50  ->
      ServerPId ! {askForMoney, Name, RemainingAmount};
    true ->
      ServerPId ! {askForMoney, Name, rand:uniform(50)}
  end,
  receive
    {ok, GrantedAmount} ->
      if
        GrantedAmount + CurrentCollected < RequestedLoan ->
          run(continue, ServerPId, Name, RequestedLoan, GrantedAmount + CurrentCollected);
        true ->
          run(stop, ServerPId, Name, RequestedLoan, CurrentCollected)
      end;
    {noFunds}->
      run(continue, ServerPId, Name, RequestedLoan, CurrentCollected);
    {teminate} -> run(stop, ServerPId, Name, RequestedLoan, CurrentCollected)
  end.