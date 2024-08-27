
# A simple implementation of bus, without references
# to any particular hardware
#

import emulator / memory

type 
  SimpleBus*[M: Memory] = ref object
    name*:   string
    id*  :   int
    ram0*:   M

type
  SoSimpleBus* = ref object
    name*: string

proc read*( bus: SimpleBus,    size: Request_Size,    address: uint32 ): uint32 {.used.} =
  case address
  of   0x0000 .. 0xFFFF:  result = bus.ram0.read(size, address)
  else:                   echo "error"
  return

proc write*( bus: SimpleBus,    size: Request_Size,    address, val: uint32 ) =
  case address
  of   0x0000 .. 0xFFFF:  bus.ram0.write(size, address, val)
  else:                   echo "error"
  return

