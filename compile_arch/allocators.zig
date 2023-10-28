const std = @import("std");
const TempAllocator = @import("TempAllocator");

pub var global_gpa = std.heap.GeneralPurposeAllocator(.{}) {};

// Never freed until exit
pub var global_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

pub var temp_arena: TempAllocator = undefined;
