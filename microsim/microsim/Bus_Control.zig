frame: arch.addr.Frame,
aa: arch.addr.Frame.Word_Offset,
ab: arch.addr.Frame.Word_Offset,

lba: bool,
uba: bool,
lbb: bool,
ubb: bool,
guard_mismatch: bool, // disregard write when this is true

const arch = @import("arch");
