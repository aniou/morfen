
import std      / logging
import std      / strformat

type
  Request_Size* = enum
    b8,
    b16le,
    b16be,
    b32le,
    b32be


proc unsupported_read_size*   (proc_name, dev_name: string,
                                            dev_id: int,
                                              mode: Request_Size,
                                           address: uint32
                              ): uint32 =
  error &"{proc_name:<12} {dev_name}{dev_id} read  {mode:<5} from  {address} not supported"
  return 0


proc unsupported_write_size*  (proc_name, dev_name: string,
                                            dev_id: int,
                                              mode: Request_Size,
                                           address: uint32,
                                               val: uint32) =
  error &"{proc_name:<12} {dev_name}{dev_id} write {mode:<5} {val} to {address} not supported"
  return

