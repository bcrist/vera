input: Min_Max,
output: Min_Max,

pub fn init(drawing: *zbox.Drawing) @This() {
    return .{
        .input = .init(drawing),
        .output = .init(drawing),
    };
}

pub const Min_Max = struct {
    min: Y_Ref,
    max: Y_Ref,

    pub fn init(drawing: *zbox.Drawing) Min_Max {
        return .{
            .min = drawing.some_y(),
            .max = drawing.some_y(),
        };
    }
};

const Y_Ref = zbox.Y_Ref;
const zbox = @import("zbox");
