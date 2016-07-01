module Constants where


-- global_mem_size = 1023

var_size = 1 :: Int

-- Fork record
fork_record_size = 30 * var_size :: Int
-- fields
-- shared fork handover record
fork_record_endp = 0 * var_size :: Int
fork_record_wr = 1 * var_size :: Int -- defaults to 0 in rest
fork_record_rd = 2 * var_size :: Int -- defaults to 1 in rest
fork_record_jump = 3 * var_size :: Int
fork_record_argc = 4 * var_size :: Int
fork_record_args = 5 * var_size :: Int
fork_record_args_size = 3 * var_size :: Int

-- Individual thread record
thread_occ = 0 * var_size :: Int

    -- number of arguments is limited to 10 due to memory size considerations.


-- Global record
global_record_size = 2 * var_size :: Int
global_record_tas = 0 * var_size :: Int
global_record_value = 1 * var_size :: Int


-- Procedure ARP

proc_arp_size = 3 * var_size :: Int -- + (number_of_variables * 3)
-- fields
proc_arp_argc = 0 * var_size :: Int

-- Proc args 
proc_arp_argrec_size = fork_record_args_size -- = 3
-- fields
proc_arp_argv = 0 * var_size :: Int
proc_arp_arg_l_addr = 1 * var_size :: Int
proc_arp_arg_s_addr = 2 * var_size :: Int

-- more fields
proc_arp_ret_addr = 1 * var_size :: Int
proc_arp_caller_arp = 2 * var_size :: Int

