
type RAM*[S: static int] = ref object
  name*: string
  id*  : int
  data*: array[S, uint8]
  size*: int


proc make_ram*(name: string, id: int, size: static int): RAM[size] =
  result      = new RAM[size]
  result.name = name
  result.id   = id
  result.size = size


proc read* [S: static int] (d: RAM[S],  mode: Request_Size,  address: uint32):  uint32 =
  case mode
  of   b8:    result = uint32(d.data[address])
  else   :    result = unsupported_read_size("read", d.name, d.id, mode, address)


proc write* [S: static int] (d: RAM[S],  mode: Request_Size,  address, value: uint32) =
  case mode
  of   b8:    d.data[address] = uint8(value)
  else   :    unsupported_write_size("write", d.name, d.id, mode, address, value)

