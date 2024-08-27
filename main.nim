
import nimprof
import std      / logging
import emulator / platforms
import emulator / memory
import emulator / common
import emulator / cpus
import streams
import strformat
#import pretty


proc hexprint(x: seq[uint8]) {.used.} =
  ##
  ## hexprint(bus0.memory[0 .. 255])
  ##
  for i in 0 .. 15:
    stdout.write fmt"{i:02x}  "
    for y in 0 .. 7:
      stdout.write fmt"{x[i*16+y]:02x} "

    stdout.write " "
    for y in 8 .. 15:
      stdout.write fmt"{x[i*16+y]:02x} "
    echo ""



proc main() =
  var log = newConsoleLogger()
  addHandler(log)
  info("a log message")

  let p = make_platform_simple()

  p.bus0.ram0.write(b8, 0, 0xEA)
  p.bus0.ram0.write(b8, 1, 0x42)
  p.bus0.ram0.write(b8, 2, 0x81)
  p.bus0.ram0.write(b8, 3, 0x81)

  let          f = "apps/6502_65C02_functional_tests.ca65/ca65/6502_functional_test.bin"
  let     stream = newFileStream(f, mode = fmRead)
  discard stream.readData(p.bus0.ram0.data.addr, 65536)
  close   stream

  p.cpu0.pc       = 0x0400
  p.cpu0.wdm_mode = true

  while not p.cpu0.abort:
    exec     p.cpu0

  echo      fmt"{p.cpu0.t8:02x}"
  hexprint  p.bus0.ram0.data[0x00 .. 0xff]

  return

main()
