module Constants where


-- global_mem_size = 1023

var_size = 1 :: Int

-- Fork record
fork_record_size = 15 * var_size :: Int

-- shared fork handover record
fork_record_endp = 0 * var_size :: Int
fork_record_wr = 1 * var_size :: Int -- defaults to 0 in rest
fork_record_rd = 2 * var_size :: Int -- defaults to 1 in rest
fork_record_jump = 3 * var_size :: Int
fork_record_argc = 4 * var_size :: Int
fork_record_args = 5 * var_size :: Int

-- Individual thread record
thread_occ = 0 * var_size :: Int

    -- number of arguments is limited to 10 due to memory size considerations.


-- Global record
global_record_size = 2 * var_size :: Int
global_record_tas = 0 * var_size :: Int
global_record_value = 1 * var_size :: Int


