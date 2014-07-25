%% A literal string
-record(literal, { value :: unicode:chardata()
                 , ignore_case :: boolean
                 , index :: index()
                 }).

-record(code, { code :: unicode:chardata()
              , index :: index()
              }).

-record(regexp, { parts       :: term()
                , raw_text    :: unicode:chardata()
                , inverted    :: boolean()
                , ignore_case :: boolean()
                , index       :: index()
                }).

%% Expression inside []
-record(charclass, { data     :: term()
                   , raw_text :: unicode:chardata()
                   , index    :: index()
                   }).

%% A range expression inside []
-record(character_range, { 'begin'  :: term()
                         , 'end'    :: term()
                         , raw_text :: unicode:chardata()
                         , index    :: index()
                         }).

%% The '.' parsing expression
-record(anything, {
                    index :: index()
                  }).

-record(rule_ref, { name  :: unicode:chardata()
                  , index :: index()
                  }).

-record(suffixed, { type       :: atom()
                  , expression :: []
                  , index      :: index()
                  }).

-record(prefixed, { type       :: atom()
                  , expression :: []
                  , code       :: #code{}
                  , index      :: index()
                  }).

-record(labeled, { label      :: unicode:chardata()
                 , expression :: []
                 , index      :: index()
                 }).

-record(text, { expression :: []
              , index      :: index()
              }).

-record(sequence, { elements :: []
                  , code     :: #code{}
                  , index    :: index()
                  }).

-record(choice, { alternatives :: []
                , index        :: index()
                }).

-record(action, { expression :: []
                , code       :: #code{}
                , index      :: index()
                }).

-record(rule, { name         :: unicode:chardata()
              , display_name :: unicode:chardata()
              , expression   :: []
              , index        :: index()
              }).

-record(grammar, { initializer :: #code{}
                 , rules       :: [#rule{}]
                 , index       :: index()
                 }).

-ifndef(index).
-define(index, {}).
-type index() :: {Line :: integer(), Column :: integer()}.
-endif.


-record(analysis, { combinators    = orddict:new()
                  , errors         = ordsets:new()
                  , required_rules = orddict:new()
                  , unique_rules   = orddict:new()
                  , code           = orddict:new()
                  , initializer    = <<>>
                  , options        = []
                  }).
