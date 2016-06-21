module Constants where


-- global_mem_size = 1023

var_size = 1

-- Thread record
thread_record_size = 14 * var_size

thread_record_wr = 0 * var_size -- defaults to 0 in rest
thread_record_rd = 1 * var_size -- defaults to 1 in rest
thread_record_jump = 2 * var_size
thread_record_argc = 3 * var_size
thread_record_args = 4 * var_size
    -- number of arguments is limited to 10 due to memory size considerations.


-- Global record
global_record_size :: Int
global_record_size = 2 * var_size
global_record_tas = 0 * var_size
global_record_value = 1 * var_size


