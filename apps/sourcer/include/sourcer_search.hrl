%% Description: References used for searches, generated by sourcer_noparse
%% Author: jakob (jakobce at g mail dot com)
%% Created: 21 mar 2010

-record(ref, {
    data, 
    offset, 
    length, 
    function, 
    arity, 
    clause, 
    sub_clause
    }).

-record(external_call, {
    module, 
    function, 
    arity
    }).
-record(local_call, {
    function, 
    arity
    }).
-record(function_def, {
    function, 
    arity
    }).
-record(function_def_mod, {
    module, 
    function, 
    arity
    }).
-record(include_ref, {
    filename
    }).
-record(macro_ref, {
    name
    }).
-record(record_ref, {
    name
    }).
-record(macro_def, {
    name
    }).
-record(record_def, {
    name
    }).
-record(type_ref, {
    module, 
    type
    }).
-record(type_def, {
    type
    }).
-record(module_def, {
    name
    }).
-record(var_def, {
    name
    }).
-record(var_ref, {
    name
    }).
-record(var_pattern, {
    vardefref, 
    function, 
    arity, 
    clause
    }).
-record(record_field_def, {
    record, 
    name
    }).
-record(record_field_ref, {
    record, 
    name
    }).
