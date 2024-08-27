
import emulator / memory
import emulator / cpus
import emulator / buses

type
  Platform*[C: CPU[Bus], B: Bus] = ref object
    cpu0*: C
    bus0*: B


type type_bus = SimpleBus[RAM[65536]]
type type_cpu = W65C02S[type_bus]

proc make_platform_simple*(): Platform[type_cpu, type_bus] =
  let p1       = new Platform[type_cpu, type_bus]
  p1.bus0      = new type_bus
  p1.cpu0      = make_W65C02S[type_bus]("cpu0", p1.bus0)
  p1.bus0.ram0 = make_ram("ram0", 0, 65536)
  return p1

