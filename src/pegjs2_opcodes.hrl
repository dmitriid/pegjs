%%% Bytecode instruction opcodes.

%%% Stack Manipulation 

-define(PUSH,             0). %% PUSH c
-define(PUSH_UNDEFINED,   1). %% PUSH_UNDEFINED
-define(PUSH_NULL,        2). %% PUSH_NULL
-define(PUSH_FAILED,      3). %% PUSH_FAILED
-define(PUSH_EMPTY_ARRAY, 4). %% PUSH_EMPTY_ARRAY
-define(PUSH_CURR_POS,    5). %% PUSH_CURR_POS
-define(POP,              6). %% POP
-define(POP_CURR_POS,     7). %% POP_CURR_POS
-define(POP_N,            8). %% POP_N n
-define(NIP,              9). %% NIP
-define(APPEND,           10). %% APPEND
-define(WRAP,             11). %% WRAP n
-define(TEXT,             12). %% TEXT

%%% Conditions and Loops

-define(IF,               13). %% IF t, f
-define(IF_ERROR,         14). %% IF_ERROR t, f
-define(IF_NOT_ERROR,     15). %% IF_NOT_ERROR t, f
-define(WHILE_NOT_ERROR,  16). %% WHILE_NOT_ERROR b

%%% Matching

-define(MATCH_ANY,        17). %% MATCH_ANY a, f, ...
-define(MATCH_STRING,     18). %% MATCH_STRING s, a, f, ...
-define(MATCH_STRING_IC,  19). %% MATCH_STRING_IC s, a, f, ...
-define(MATCH_REGEXP,     20). %% MATCH_REGEXP r, a, f, ...
-define(ACCEPT_N,         21). %% ACCEPT_N n
-define(ACCEPT_STRING,    22). %% ACCEPT_STRING s
-define(FAIL,             23). %% FAIL e

%%% Calls

-define(LOAD_SAVED_POS,   24). %% LOAD_SAVED_POS p
-define(UPDATE_SAVED_POS, 25). %% UPDATE_SAVED_POS
-define(CALL,             26). %% CALL f, n, pc, p1, p2, ..., pN

%%% Rules

-define(RULE,             27). %% RULE r

%%% Failure Reporting

-define(SILENT_FAILS_ON,  28). %% SILENT_FAILS_ON
-define(SILENT_FAILS_OFF, 29). %% SILENT_FAILS_OFF
