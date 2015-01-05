-define(F(X), fun() -> X end).

-define( whenFail(OnFail, Condition)
       , try
           Condition
         catch
           _:___Reason ->
             OnFail,
             error(___Reason)
         end
       ).

-define(DEBUG(Format, Data), io:format(user, Format, Data)).