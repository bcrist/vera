pub const JLow = u16;
pub const JHigh = u16;
pub const J = u32;
pub const JParts = packed struct {
    low: JLow,
    high: JHigh,

    pub const zero = JParts{
        .low = 0,
        .high = 0,
    };
};

pub const K = u16;

pub const LLow = u16;
pub const LHigh = u16;
pub const L = u32;
pub const LParts = packed struct {
    low: LLow,
    high: LHigh,

    pub const zero = LParts{
        .low = 0,
        .high = 0,
    };
};

pub const D = u16;

pub const Page = u20;
pub const PageOffset = u12;
pub const Frame = u12;

pub const VirtualAddress = u32;
pub const VirtualAddressParts = packed struct {
    offset: PageOffset,
    page: Page,

    pub const zero = VirtualAddressParts{
        .offset = 0,
        .page = 0,
    };
};

pub const PhysicalAddress = u24;
pub const PhysicalAddressParts = packed struct {
    offset: PageOffset,
    frame: Frame,

    pub const zero = PhysicalAddressParts{
        .offset = 0,
        .frame = 0,
    };
};
