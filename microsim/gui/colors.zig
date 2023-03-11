pub const transparent = [_]f32 { 0, 0, 0, 0 };
pub const white = [_]f32 { 1.0, 1.0, 1.0, 1.0 };
pub const alpha_gray = [_]f32 { 1.0, 1.0, 1.0, 0.7};

pub const label = [_]f32 { 0.4, 0.4, 0.4, 1.0 };
pub const read = [_]f32 { 0.2, 0.7, 0.2, 1.0 };
pub const write = [_]f32 { 0.7, 0.4, 0.2, 1.0 };

pub const stage_setup = [_]f32 { 0.5, 1.0, 0.5, 1.0 };
pub const stage_compute = [_]f32 { 0.5, 0.7, 1.0, 1.0 };
pub const stage_transact = [_]f32 { 1.0, 0.7, 0.5, 1.0 };

pub const exec_normal = [_]f32 { 0.8, 0.8, 0.8, 1.0 };
pub const exec_interrupt = [_]f32 { 0.8, 0.8, 0.0, 1.0 };
pub const exec_fault = [_]f32 { 1.0, 0.4, 0.4, 1.0 };
