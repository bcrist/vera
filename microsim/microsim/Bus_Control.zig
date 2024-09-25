frame: arch.addr.Frame,
aa: arch.addr.Word_Offset,
ab: arch.addr.Word_Offset,

lba: bool,
uba: bool,
lbb: bool,
ubb: bool,
guard_mismatch: bool, // disregard write when this is true
block_transfer: bool,
update_frame_state: bool,

const arch = @import("arch");
