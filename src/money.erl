-module(money).
-author("Wasim Alayoubi").

%% API
-export([start/0, server/3]).

start()->
  CustomersTupleList = readCustomersFromFile(),
  BanksTupleList = readBanksFromFile(),
  io:fwrite("** Customers and loan objectives **~n"),
  lists:map(fun(CustomerTuple) -> printCustomersLoanObjectives(CustomerTuple)  end, CustomersTupleList),
  io:fwrite("** Banks and financial resources **~n"),
  lists:map(fun(BankTuple) -> printBanksFinancialResources(BankTuple)  end, BanksTupleList),
  BanksPidMap =  #{"x"=>-1},
  CustomersPidMap =  #{"x"=>-1},
  MoneyServerPId = spawn(money, server, [start, BanksPidMap, CustomersPidMap]),
  lists:map(fun(BankTuple) -> createBankProcess(BankTuple, MoneyServerPId) end, BanksTupleList),
  lists:map(fun(CustomerTuple) -> createCustomerProcess(CustomerTuple, MoneyServerPId) end, CustomersTupleList),
  Starting = "Starting ....",
  Starting.


readCustomersFromFile()->
  {ok, CustomersTupleList} = file:consult("Customers.txt"),
  CustomersTupleList.

readBanksFromFile()->
  {ok, BanksTupleList} = file:consult("Banks.txt"),
  BanksTupleList.

printCustomersLoanObjectives(CustomerTuple)->
  Name = element(1,CustomerTuple),
  RequestedLoan = element(2,CustomerTuple),
  io:fwrite("~w: ~w.~n", [Name,RequestedLoan]).

printBanksFinancialResources(BankTuple)->
  Name = element(1,BankTuple),
  AvailableFund = element(2,BankTuple),
  io:fwrite("~w: ~w.~n", [Name,AvailableFund]).

createCustomerProcess(CustomerTuple, MoneyServerPId)->
  Name = element(1,CustomerTuple),
  RequestedLoan = element(2,CustomerTuple),
  spawn(customer, run, [new, MoneyServerPId, Name, RequestedLoan, 0]).

createBankProcess(BankTuple, MoneyServerPId)->
  Name = element(1,BankTuple),
  AvailableFund = element(2,BankTuple),
  spawn(bank, run, [new, MoneyServerPId, Name, AvailableFund]).


terminate([]) ->
  done;
terminate(PIdsList) ->
  [Head | Tail] = PIdsList,
  Head ! {terminate},
  receive
    {messageRO, CustomerName, CurrentCollected} ->
      io:fwrite("~w was only able to borrow ~w dollar(s). Boo Hoo! ~n", [CustomerName, CurrentCollected]),
      terminate(Tail);
    {unregisterCustomer, ""} ->
      terminate(Tail);
    {messagehd, BankName, AvailableFund}->
      io:fwrite("~w has ~w dollar(s) remaining.~n", [BankName, AvailableFund]),
      terminate(Tail);
    {unregisterBank, ""}->
      terminate(Tail)
  end.

server(breakByCustomers, BanksPIdMap, #{}) ->
  %io:fwrite("Inside breakByCustomers -> BanksPIdMap >>>>>>~p>>>>size:~w~n",[BanksPIdMap, map_size(BanksPIdMap)]),
  terminate(maps:values(BanksPIdMap)),
  done;
server(breakByBanks, #{}, CustomersPIdMap) ->
  %io:fwrite("Inside breakByBanks -> BanksPIdMap >>>>>>~p>>>>size:~w~n",[CustomersPIdMap, map_size(CustomersPIdMap)]),
  terminate(maps:values(CustomersPIdMap)),
  done;
server(start, BanksPIdMap, CustomersPIdMap) ->
  server(continue, BanksPIdMap, CustomersPIdMap);
server(continue, BanksPIdMap, CustomersPIdMap) ->
  timer:sleep(rand:uniform(1000)),
  %io:fwrite("BanksPIdMap >>>>>>~p>>>>size:~w~n",[BanksPIdMap, map_size(BanksPIdMap)]),
  %io:fwrite("CustomersPIdMap >>>>>~p>>>>size:~w~n",[CustomersPIdMap, map_size(CustomersPIdMap)]),
  if
    map_size(BanksPIdMap) == 0 ->
      server(breakByBanks, BanksPIdMap, CustomersPIdMap);
    true ->
      io:fwrite("")
  end,
  if
    map_size(CustomersPIdMap) == 0 ->
      server(breakByCustomers, BanksPIdMap, CustomersPIdMap);
    true ->
      io:fwrite("")
  end,
  receive
    {registerCustomer, CustomerName, CustomerPId} ->
      server(continue, BanksPIdMap, maps:put(CustomerName, CustomerPId, maps:remove("x",CustomersPIdMap)));
    {registerBank, BankName, BankPId} ->
      server(continue, maps:put(BankName, BankPId, maps:remove("x",BanksPIdMap)), CustomersPIdMap);
    {unregisterBank, BankName} ->
      server(continue, maps:remove(BankName, BanksPIdMap), CustomersPIdMap);
    {unregisterCustomer, CustomerName} ->
      server(continue, BanksPIdMap, maps:remove(CustomerName, CustomersPIdMap));
    {askForMoney, CustomerName, RequestedAmount} ->
      if
        map_size(BanksPIdMap) /= 0 ->
          RandomBankName = lists:nth(rand:uniform(map_size(BanksPIdMap)), maps:keys(BanksPIdMap)),
          RandomBankPId = maps:get(RandomBankName, BanksPIdMap),
          io:fwrite("~w requests a loan of ~w dollar(s) from ~w~n", [CustomerName, RequestedAmount, RandomBankName]),
          RandomBankPId ! {requestMoney, CustomerName, RequestedAmount};
        true ->
          CustomerPId = maps:get(CustomerName, CustomersPIdMap),
          CustomerPId ! {terminate}
      end,
      server(continue, BanksPIdMap, CustomersPIdMap);
    {moneyGranted, BankName, CustomerName, GrantedAmount} ->
      io:fwrite("~w approves a loan of ~w dollars from ~w~n", [BankName, GrantedAmount, CustomerName]),
      CustomerPId = maps:get(CustomerName, CustomersPIdMap),
      CustomerPId ! {ok, GrantedAmount},
      server(continue, BanksPIdMap, CustomersPIdMap);
    {moneyGrantedNotInFull, BankName, CustomerName, RequestedAmount, GrantedAmount} ->
      io:fwrite("~w denies a loan of ~w dollars from ~w~n", [BankName, RequestedAmount, CustomerName]),
      io:fwrite("~w approves a partial amount of ~w dollars from ~w~n", [BankName, GrantedAmount, CustomerName]),
      CustomerPId = maps:get(CustomerName, CustomersPIdMap),
      CustomerPId ! {ok, GrantedAmount},
      server(continue, BanksPIdMap, CustomersPIdMap);
    {messageRO, CustomerName, RequestedLoan} ->
      io:fwrite("~w has reached the objective of ~w dollar(s). Woo Hoo! ~n", [CustomerName, RequestedLoan]),
      server(continue, BanksPIdMap, CustomersPIdMap);
    {messagehd, BankName, AvailableFund} ->
      io:fwrite("~w has ~w dollar(s) remaining.~n", [BankName, AvailableFund]),
      server(continue, BanksPIdMap, CustomersPIdMap);
    {bankIsOut, BankName, CustomerName, RequestedAmount}->
      io:fwrite("~w denies a loan of ~w dollars from ~w, No more funds!~n", [BankName, RequestedAmount, CustomerName]),
      CustomerPId = maps:get(CustomerName, CustomersPIdMap),
      CustomerPId ! {noFunds},
      server(continue, BanksPIdMap, CustomersPIdMap)
  end.