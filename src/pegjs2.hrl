-record(entry, { type :: binary()
               , name :: binary()
               , display_name :: binary()
               , label :: binary()
               , rules :: list()
               , expression :: list()
               , alternatives :: list()
               , elements :: list()
               , code :: iolist()
               , parts :: iolist()
               , inverted :: boolean()
               , ignore_case :: boolean()
               , raw_text :: iolist() | binary()
               , value :: binary()
               , initializer :: tuple()
               , index :: index()
               , description :: binary()
               }).

-ifndef(index).
-define(index, {}).
-type index() :: {Line :: integer(), Column :: integer()}.
-endif.


-record(analysis, { combinators    = dict:new()
                  , errors         = dict:new()
                  , required_rules = dict:new()
                  , unique_rules   = dict:new()
                  , code           = dict:new()
                  , initializer    = <<>>
                  , options        = []
                  , grammar        = #entry{}
                  }).
