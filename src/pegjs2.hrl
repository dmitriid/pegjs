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
               , inverted = false:: boolean()
               , ignore_case = false :: boolean()
               , raw_text :: iolist() | binary()
               , value :: binary()
               , initializer :: tuple()
               , index :: index()
               , description :: binary()
               }).

-record(function, { arg = <<>>
                  , code = <<>>
                  , index = {{line, 1}, {column, 1}}
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
                  , data           = []
                  }).
