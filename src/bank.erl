-module(bank).
-author("Wasim Alayoubi").
%% API
-export([run/4]).

run(stop, ServerPId, Name, AvailableFund)->
  ServerPId ! {messagehd, Name, AvailableFund},
  ServerPId ! {unregisterBank, Name},
  receive
    {requestMoney, CustomerName, RequestedAmount} ->
      ServerPId ! {bankIsOut, Name, CustomerName, RequestedAmount},
      run(stop, ServerPId, Name, AvailableFund)
  end;
run(new, ServerPId, Name, AvailableFund)->
  ServerPId ! {registerBank, Name, self()},
  run(continue, ServerPId, Name, AvailableFund);
run(continue, ServerPId, Name, AvailableFund) ->
  timer:sleep(rand:uniform(3000)),
  receive
    {requestMoney, CustomerName, RequestedAmount} ->
      if
        AvailableFund >= RequestedAmount ->
          ServerPId ! {moneyGranted, Name, CustomerName, RequestedAmount},
          run(continue, ServerPId, Name, AvailableFund - RequestedAmount);
        true ->
          ServerPId ! {moneyGrantedNotInFull, Name, CustomerName, RequestedAmount, AvailableFund},
          run(stop, ServerPId, Name, 0)
      end;
    {terminate} -> run(stop, ServerPId, Name, AvailableFund)
  end.