pub const transparent = ig.Color.transparent;
pub const white = ig.Color.white;
pub const alpha_gray = ig.Color.init_rgba(1, 1, 1, 0.7);

pub const label = ig.Color.init_rgb(0.4, 0.4, 0.4);
pub const read  = ig.Color.init_rgb(0.2, 0.7, 0.2);
pub const write = ig.Color.init_rgb(0.7, 0.4, 0.2);

pub const stage_reset    = ig.Color.init_rgb(1.0, 0.5, 0.5);
pub const stage_decode   = ig.Color.init_rgb(0.7, 0.5, 1.0);
pub const stage_setup    = ig.Color.init_rgb(0.5, 0.7, 1.0);
pub const stage_compute  = ig.Color.init_rgb(0.5, 1.0, 0.5);
pub const stage_transact = ig.Color.init_rgb(1.0, 0.7, 0.5);

pub const exec_normal    = ig.Color.init_rgb(0.8, 0.8, 0.8);
pub const exec_interrupt = ig.Color.init_rgb(0.8, 0.8, 0.0);
pub const exec_fault     = ig.Color.init_rgb(1.0, 0.4, 0.4);

const ig = @import("ig");
