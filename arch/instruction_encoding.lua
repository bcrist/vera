-- This file is deprecated; due to issues with LLVM compilation speed,
-- this data is now loaded from instruction_encoding.sx by zig code at runtime.

local empty_params = {}

local write = write
local nl = nl
local indent = indent
local unindent = unindent
local set_indent = set_indent
local sx = sx
local template = template

local param_property_visitor = {
    ['base-src'] = function(parser, _, insn, param)
        param.base_src = parser:require_string()
        parser:require_close()
        return param.base_src
    end,

    ['offset-src'] = function(parser, _, insn, param)
        param.offset_src = parser:require_string()
        parser:require_close()
        return param.offset_src
    end,

    rev = function(parser, _, insn, param)
        param.rev = true
        parser:require_close()
        return true
    end,

    reg = function(parser, _, insn, param)
        param.min_reg = parser:require_unsigned()
        param.max_reg = parser:require_unsigned()
        parser:require_close()
        return nil
    end,

    range = function(parser, _, insn, param)
        local ranges = param.ranges
        if ranges == nil then
            ranges = {}
            param.ranges = ranges
        end
        local range = {}
        range.min = parser:require_int()
        range.max = parser:require_int()
        ranges[#ranges + 1] = range
        parser:require_close()
        return nil
    end,

    ['alt-range'] = function(parser, _, insn, param)
        local ranges = param.alt_ranges
        if ranges == nil then
            ranges = {}
            param.alt_ranges = ranges
        end
        local range = {}
        range.min = parser:require_int()
        range.max = parser:require_int()
        ranges[#ranges + 1] = range
        parser:require_close()
        return nil
    end,

    align = function(parser, _, insn, param)
        param.align = parser:require_unsigned()
        parser:require_close()
        return param.align
    end,
}

local instruction_property_visitor = {
    desc = function (parser, _, insn)
        insn.desc = parser:require_string()
        parser:require_close()
        return insn.desc
    end,

    ['opcode-base'] = function (parser, _, insn)
        insn.opcode_base = parser:require_unsigned(16)
        insn:require_close()
        return insn.opcode_base
    end,

    param = function(parser, _, insn)
        local p = {}
        local index = #insn.params + 1

        p.arrow = parser:string('->') or nil
        p.base_type = parser:require_string()
        p.offset_type = parser:string()
        while nil ~= parser:property(param_property_visitor, insn, p) do end
        parser:require_close()

        if index == 1 then
            insn.params = {}
        end
        insn.params[index] = p
        return p
    end,
}

function parseInstruction (parser)
    if not parser:open() then return end

    local insn = {
        params = empty_params
    }

    insn.min_opcode = parser:require_unsigned(16)
    insn.max_opcode = parser:require_unsigned(16)
    insn.mnemonic = parser:require_string()
    insn.suffix = parser:string()
    while nil ~= parser:property(instruction_property_visitor, insn) do end
    parser:require_close()

    return insn
end

function parseInstructions (str)
    local by_opcode = {}
    local by_mnemonic = {}
    local parser = sx.parser(str)

    parser:require_open()
    for insn in parseInstruction, parser do
        local mnemonic_list = by_mnemonic[insn.mnemonic]
        if mnemonic_list == nil then
            mnemonic_list = { insn }
            by_mnemonic[insn.mnemonic] = mnemonic_list
        else
            mnemonic_list[#mnemonic_list + 1] = insn
        end
        for n = insn.min_opcode, insn.max_opcode do
            by_opcode[n] = insn
        end
    end
    parser:require_close()
    parser:require_done()

    write(parser:array())

    return by_opcode, by_mnemonic
end

local function writeConstantRanges(ranges, field_name)
    if ranges ~= nil then
        write(nl, '.', field_name)
        if #ranges == 1 then
            write(' = constantRange(', ranges[1].min, ', ', ranges[1].max, '),')
        elseif #ranges == 2 and ranges[1].min == 0 and ranges[2].max == -1 then
            write(' = signedConstantRange(', ranges[2].min, ', ', ranges[1].max, '),')
        else
            write(' = &[_]ConstantRange {', indent)
            for i = 1, #ranges do
                local range = ranges[i]
                write(nl, '.{ .min = ', range.min, ', .max = ', range.max, ' },')
            end
            write(unindent, nl, '},')
        end
    end
end

function writeParameterEncodingLiteral(param)
    write(nl, indent, '.{ ')
    if param.arrow then
        write(nl, '.arrow = true,')
    end

    write(nl, '.type = .{ .base = .', param.base_type)
    if param.offset_type then
        write(', .offset = .', param.offset_type)
    end
    write(' },')

    if param.base_src   then write(nl, '.base_src = .', param.base_src, ',') end
    if param.offset_src then write(nl, '.offset_src = .', param.offset_src, ',') end
    if param.min_reg    then write(nl, '.min_reg = ', param.min_reg, ',') end
    if param.max_reg    then write(nl, '.max_reg = ', param.max_reg, ',') end
    if param.rev        then write(nl, '.constant_reverse = true,') end
    if param.align      then write(nl, '.constant_align = ', param.align, ',') end

    writeConstantRanges(param.ranges, 'constant_ranges')
    writeConstantRanges(param.alt_ranges, 'alt_constant_ranges')
    write(unindent, nl, '},')
end

function hex_opcode(opcode)
    return ('%04X'):format(opcode)
end

writeInsnEncodingConstant = template [=[`
min_hex = hex_opcode(min_opcode)
max_hex = hex_opcode(max_opcode)
`
const `mnemonic`_`min_hex` = InstructionEncoding {
    .mnemonic = .`mnemonic`,
    .suffix = .`_X.suffix or 'none'`,
    .desc = `('%q'):format(desc)`,
    .opcode_base = 0x`hex_opcode(_X.opcode_base or min_opcode)`,
    .opcodes = .{ .min = 0x`min_hex`, .max = 0x`max_hex` },
    .params = &[_]ParameterEncoding {`
local n = #params
if n > 0 then
    indent(2)
    for i = 1, #params do
        writeParameterEncodingLiteral(params[i])
    end
    unindent()
    nl()
    unindent()
end
    `},
};]=]

function writeGetMatchingEncodings(mnemonic, instructions)
    set_indent(0)
    write(nl, 'fn getMatchingEncodings_', mnemonic, '(insn: Instruction) InstructionEncodingIterator {', indent)

    write(nl, 'return InstructionEncodingIterator.init(insn, &[_]InstructionEncoding {', indent)
    for i = 1, #instructions do
        local insn = instructions[i]
        write(nl, insn.mnemonic, '_', hex_opcode(insn.min_opcode), ',')
    end
    unindent()
    write(nl, '});', unindent)
    write(nl, '}')
end



function writeExtractEncoding(instructions_by_opcode)
    write('switch (op >> 8) {', indent)

    local groups = {}

    local opcode = 0
    while opcode < 0x10000 do
        local insn = instructions_by_opcode[opcode]
        if insn == nil then
            opcode = opcode + 1
        else
            local min_group = insn.min_opcode >> 8
            local max_group = insn.max_opcode >> 8
            for g = min_group, max_group do
                local group = groups[g]
                if group == nil then
                    group = {}
                    groups[g] = group
                end
                group[#group + 1] = insn
            end
            opcode = insn.max_opcode + 1
        end
    end

    for g = 0, 0xFF do
        local group = groups[g]
        write(nl, ('0x%02X => '):format(g))
        if group == nil or #group == 0 then
            write 'return error.InvalidInstruction,'
        elseif #group == 1 then
            local insn = group[1]

            local min = insn.min_opcode & 0xFF
            local max = insn.max_opcode & 0xFF

            local min_hex = hex_opcode(insn.min_opcode)
            local max_hex = hex_opcode(insn.max_opcode)

            if min == 0 and max == 0xFF then
                write('return ', insn.mnemonic, '_', min_hex, ',')
            elseif (insn.min_opcode == insn.max_opcode) then
                write('return if (op == 0x', min_hex, ') return ', insn.mnemonic, '_', min_hex, ' else error.InvalidInstruction,')
            else
                write('return if (op >= 0x', min_hex, ' and op <= 0x', max_hex, ') ', insn.mnemonic, '_', min_hex, ' else error.InvalidInstruction,')
            end
        else
            write('switch (@truncate(u4, op >> 4)) {', indent)

            local subgroups = {}
            for i = 1, #group do
                local insn = group[i]
                local min_sg = (insn.min_opcode >> 4) & 0xF
                local max_sg = (insn.max_opcode >> 4) & 0xF
                for sg = min_sg, max_sg do
                    local subgroup = subgroups[sg]
                    if subgroup == nil then
                        subgroup = {}
                        subgroups[sg] = subgroup
                    end
                    subgroup[#subgroup + 1] = insn
                end
            end

            for sg = 0, 15 do
                write(nl, ('0x%X'):format(sg), ' => ')
                local subgroup = subgroups[sg]
                local min_opcode = (g << 8) | (sg << 4)
                local max_opcode = min_opcode + 15

                if subgroup == nil or #subgroup == 0 then
                    write 'return error.InvalidInstruction,'
                elseif #subgroup == 1 then
                    local insn = subgroup[1]
                    local min_hex = hex_opcode(insn.min_opcode)
                    local max_hex = hex_opcode(insn.max_opcode)
                    if insn.min_opcode <= min_opcode and insn.max_opcode >= max_opcode then
                        write('return ', insn.mnemonic, '_', min_hex, ',')
                    elseif insn.min_opcode == insn.max_opcode then
                        write('return if (op == 0x', min_hex, ') return ', insn.mnemonic, '_', min_hex, ' else error.InvalidInstruction,')
                    elseif insn.min_opcode <= min_opcode then
                        write('return if (op <= 0x', max_hex, ') return ', insn.mnemonic, '_', min_hex, ' else error.InvalidInstruction,')
                    elseif insn.max_opcode >= max_opcode then
                        write('return if (op >= 0x', min_hex, ') return ', insn.mnemonic, '_', min_hex, ' else error.InvalidInstruction,')
                    else
                        write('return if (op >= 0x', min_hex, ' and op <= 0x', max_hex, ') return ', insn.mnemonic, '_', min_hex, ' else error.InvalidInstruction,')
                    end
                else
                    write('switch (@truncate(u4, op)) {', indent)
                    local ops = {}
                    for i = 1, #subgroup do
                        local insn = subgroup[i]
                        for op = min_opcode, max_opcode do
                            if insn.min_opcode <= op and op <= insn.max_opcode then
                                ops[op] = insn
                            end
                        end
                    end

                    for ssg = 0, 15 do
                        write(nl, ('0x%X'):format(ssg), ' => ')
                        local opcode = min_opcode + ssg
                        local insn = ops[opcode]
                        if insn == nil then
                            write 'return error.InvalidInstruction,'
                        else
                            write('return ', insn.mnemonic, '_', hex_opcode(insn.min_opcode), ',')
                        end
                    end
                    --write(nl, 'else => unreachable')
                    write(unindent, nl, '},')
                end
            end
            --write(nl, 'else => unreachable,')
            write(unindent, nl, '},')
        end
    end
    write(nl, 'else => unreachable,')
    write(unindent, nl, '}')
end