%%% Bytecode instruction opcodes.

%%  Stack Manipulation 

-define(PUSH,              0).  %% PUSH c
-define(PUSH_UNDEFINED,   26). %% PUSH_UNDEFINED
-define(PUSH_NULL,        27). %% PUSH_NULL
-define(PUSH_FAILED,      28). %% PUSH_FAILED
-define(PUSH_EMPTY_ARRAY, 29). %% PUSH_EMPTY_ARRAY
-define(PUSH_CURR_POS,     1). %% PUSH_CURR_POS
-define(POP,               2). %% POP
-define(POP_CURR_POS,      3). %% POP_CURR_POS
-define(POP_N,             4). %% POP_N n
-define(NIP,               5). %% NIP
-define(APPEND,            6). %% APPEND
-define(WRAP,              7). %% WRAP n
-define(TEXT,              8). %% TEXT

%%  Conditions and Loops 

-define(IF,               9). %% IF t, f
-define(IF_ERROR,        10). %% IF_ERROR t, f
-define(IF_NOT_ERROR,    11). %% IF_NOT_ERROR t, f
-define(WHILE_NOT_ERROR, 12). %% WHILE_NOT_ERROR b

%%  Matching 

-define(MATCH_ANY,       13). %% MATCH_ANY a, f, ...
-define(MATCH_STRING,    14). %% MATCH_STRING s, a, f, ...
-define(MATCH_STRING_IC, 15). %% MATCH_STRING_IC s, a, f, ...
-define(MATCH_REGEXP,    16). %% MATCH_REGEXP r, a, f, ...
-define(ACCEPT_N,        17). %% ACCEPT_N n
-define(ACCEPT_STRING,   18). %% ACCEPT_STRING s
-define(FAIL,            19). %% FAIL e

%%  Calls 

-define(REPORT_SAVED_POS, 20). %% REPORT_SAVED_POS p
-define(REPORT_CURR_POS,  21). %% REPORT_CURR_POS
-define(CALL,             22). %% CALL f, n, pc, p1, p2, ..., pN

%%  Rules 

-define(RULE, 23). %% RULE r

%%  Failure Reporting 

-define(SILENT_FAILS_ON,  24). %% SILENT_FAILS_ON
-define(SILENT_FAILS_OFF, 25). %% SILENT_FAILS_FF
