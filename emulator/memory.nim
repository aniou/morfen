
import emulator / common

include 
  memory_ram_simple

# just for test, XXX - remove it
type OtherRAM* = ref object
  data*: array[32, uint8]

type
  Memory* = RAM|OtherRam

