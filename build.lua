-- This file is used to generate build.zig, based on the configuration in build.sx

local parser = sx.parser(get_file_contents('build.sx'))
local root_visitor = {}
local dir_visitor = {}
local dep_visitor = {}
local exe_visitor = {}
local module_visitor = {}

local dirs = {}
local dep_pkgs = {}
local notest_dirs = {}
local file_deps = {}
local modules = {}
local exes = {}
local tests = {}

local ignored_imports = {
    std = true,
    builtin = true,
    root = true,
}

function make_safe_name (name)
    return name:gsub('[^A-Za-z0-9_]', '_'):gsub('^test$', '_test')
end

function root_visitor.dir ()
    local path = parser:require_string()
    dirs[#dirs + 1] = path
    while nil ~= parser:property(dir_visitor, path) do end
    parser:require_close()
end

function root_visitor.dep ()
    local name = parser:require_string()
    local dep = {
        name = name,
        safe_name = 'dep__' .. make_safe_name(name),
    }
    dep_pkgs[name] = dep
    while nil ~= parser:property(dep_visitor, dep) do end
    parser:require_close()
end

-- Comments
function root_visitor._ ()
    parser:ignore_remaining_expression()
end
function dir_visitor._ ()
    parser:ignore_remaining_expression()
end
function dep_visitor._ ()
    parser:ignore_remaining_expression()
end

function root_visitor.module ()
    local name = parser:require_string()
    local module = {
        name = name,
        safe_name = make_safe_name(name),
        raw = true,
        extra_exe_config = {},
        pass_exe_to = {},
    }
    while nil ~= parser:property(module_visitor, module) do end
    parser:require_close()
    modules[module.name] = module
end

function dep_visitor.module (_, _, dep_pkg)
    local name = parser:require_string()
    local module = {
        name = name,
        safe_name = make_safe_name(name),
        dep_pkg = dep_pkg,
        extra_exe_config = {},
        pass_exe_to = {},
    }
    while nil ~= parser:property(module_visitor, module) do end
    parser:require_close()
    modules[module.name] = module
end

function dir_visitor.module (_, _, dir_path)
    local name = parser:require_string()
    local default_path = fs.compose_path_slash(dir_path, fs.replace_extension(name, '.zig'))
    local module = {
        name = name,
        safe_name = make_safe_name(name),
        dir = dir_path,
        path = default_path,
        extra_exe_config = {},
        pass_exe_to = {},
    }
    while nil ~= parser:property(module_visitor, modules) do end
    parser:require_close()
    modules[module.name] = module
end

function dir_visitor.exe (_, _, dir_path)
    local name = parser:require_string()
    local default_path = fs.compose_path_slash(dir_path, name .. '.zig')
    local executable = {
        name = name,
        safe_name = make_safe_name(name),
        dir = dir_path,
        path = default_path,
        extra_exe_config = {},
        pass_exe_to = {},
    }
    while nil ~= parser:property(exe_visitor, executable) do end
    parser:require_close()
    exes[executable.name] = executable
end

function dir_visitor.notest (_, _, dir_path)
    parser:require_close()
    notest_dirs[dir_path] = true
end

function module_visitor.path (_, _, module)
    local filename = fs.replace_extension(parser:require_string(), '.zig')
    parser:require_close()
    module.path = fs.compose_path_slash(module.dir, filename)
end

function module_visitor.func (_, _, module)
    module.pass_exe_to[#module.pass_exe_to+1] = parser:require_string()
    parser:require_close()
end

function module_visitor.config (_, _, module)
    module.extra_exe_config[#module.extra_exe_config+1] = parser:require_string()
    parser:require_close()
end

function module_visitor.safe_name (_, _, module)
    local safe_name = parser:require_string()
    parser:require_close()
    module.safe_name = safe_name
end

function exe_visitor.run_step (_, _, executable)
    executable.run_step = parser:require_string()
    parser:require_close()
end

exe_visitor.func = module_visitor.func
exe_visitor.config = module_visitor.config
exe_visitor.safe_name = module_visitor.safe_name

while nil ~= parser:property(root_visitor) do end

-- Look for file dependencies and tests within .zig files
for i = 1, #dirs do
    local dir = dirs[i]
    local check_for_tests = not notest_dirs[dir]
    fs.visit(dir, function(subpath, kind)
        if kind ~= 'File' or fs.path_extension(subpath) ~= '.zig' then return end

        local path = fs.compose_path_slash(dir, subpath)
        local contents = get_file_contents(path)
        local deps = {}
        for import_name in contents:gmatch('@import%("([^"]+)"%)') do
            if fs.path_extension(import_name) == '.zig' then
                deps[#deps + 1] = fs.compose_path_slash(dir, import_name)
            elseif ignored_imports[import_name] == nil then
                deps[#deps + 1] = import_name
            end
        end
        if #deps > 0 then
            file_deps[path] = deps
        end

        if check_for_tests and (contents:match('\ntest%s*"[^"]*"%s*{') or contents:match('\ntest%s*{')) then
            tests[path] = true
        end
    end)
end

local function collect_named_deps (path, named_deps, visited_files)
    local deps = file_deps[path]
    if deps == nil then return end

    if visited_files[path] ~= nil then return end
    visited_files[path] = true

    for i = 1, #deps do
        local dep = deps[i]
        if fs.path_extension(dep) == '.zig' then
            collect_named_deps(dep, named_deps, visited_files)
        else
            if modules[dep] == nil then
                error("Module not found: " .. dep)
            end
            named_deps[dep] = true
            modules[dep].used = true
        end
    end
end

-- Compile dependencies for each module
for _, module in pairs(modules) do
    if module.deps == nil then
        module.deps = {}
        if not module.raw and not module.dep_pkg then
            collect_named_deps(module.path, module.deps, {})
        end
    end
end

-- Compile dependencies for each executable
for _, exe in pairs(exes) do
    if exe.deps == nil then
        exe.deps = {}
        collect_named_deps(exe.path, exe.deps, {})
    end
    if exe.run_step == nil then
        exe.run_step = exe.name
    end
end

local function get_module (name)
    local module = modules[name]
    if module == nil then
        error("Module " .. name .. " not found!")
    end
    return module
end

local function try_write_module (module)
    if module.raw then return end

    if not module.written_to_build then
        if module.dep_pkg then
            if not module.dep_pkg.written_to_build then
                writeln(nl, 'const ', module.dep_pkg.safe_name, ' = b.dependency("', module.dep_pkg.name, '", .{});')
                module.dep_pkg.written_to_build = true
            end
            writeln(nl, 'const ', module.safe_name, ' = ', module.dep_pkg.safe_name, '.module("', module.name, '");')
            module.written_to_build = true
            return
        end

        if module.started_writing_to_build then
            if not module.written_to_build_without_dependencies then
                -- There's a circular dependency; we need a forward declaration to avoid infinite recursion
                write(nl, 'const ', module.safe_name, ' = b.createModule(.{', indent)
                write(nl, '.source_file = .{ .path = "', module.path, '" },')
                unindent()
                nl()
                write '});'
                nl()
                module.written_to_build_without_dependencies = true
            end
            return
        end

        module.started_writing_to_build = true
        for dep in spairs(module.deps) do
            try_write_module(get_module(dep))
        end
        if module.written_to_build_without_dependencies then
            if next(module.deps) then
                nl()
                for dep in spairs(module.deps) do
                    local safe_name = modules[dep].safe_name
                    writeln(module.safe_name, '.dependencies.put("', dep, '", ', safe_name, ') catch unreachable;')
                end
            end
        else
            write(nl, 'const ', module.safe_name, ' = b.createModule(.{', indent)
            write(nl, '.source_file = .{ .path = "', module.path, '" },')
            if next(module.deps) then
                nl()
                write '.dependencies = &.{'
                indent()
                for dep in spairs(module.deps) do
                    local safe_name = modules[dep].safe_name
                    write(nl, '.{ .name = "', dep, '", .module = ', safe_name, ' },')
                end
                unindent()
                nl()
                write '},'
            end
            unindent()
            nl()
            write '});'
            nl()
        end

        module.written_to_build = true
    end
end

for _, module in spairs(modules) do
    try_write_module(module)
end

local write_exe = template [[

const `safe_name` = b.addExecutable(.{
    .name = "`name`",
    .root_source_file = .{ .path = "`path`" },
    .target = target,
    .optimize = mode,
});`
local modules = ...
for dep in spairs(deps) do
    local module = modules[dep]
    write(nl, safe_name, '.addModule("', dep, '", ', module.safe_name,');')
    for _, fun in ipairs(module.extra_exe_config) do 
        write(nl, safe_name, '.', fun, ';')
    end
    for _, fun in ipairs(module.pass_exe_to) do
        write(nl, fun, '(', safe_name, ');')
    end
end
for _, fun in ipairs(extra_exe_config) do 
    write(nl, safe_name, '.', fun, ';')
end
for _, fun in ipairs(pass_exe_to) do
    writeln(nl, fun, '(', safe_name, ');')
end`
b.installArtifact(`safe_name`);
_ = makeRunStep(b, `safe_name`, "`run_step`", "Run `name`");
]]

for _, exe in spairs(exes) do
    write_exe(exe, modules)
end

local i = 1
for test in spairs(tests) do
    local deps = {}
    collect_named_deps(test, deps, {})

    write(nl, 'const tests', i, ' = b.addTest(.{')
    indent()
    write(nl, '.root_source_file = .{ .path = "', test, '"},')
    write(nl, '.target = target,')
    write(nl, '.optimize = mode,')
    unindent()
    nl()
    write('});')
    for dep in spairs(deps) do
        write(nl, 'tests', i, '.addModule("', dep, '", ', modules[dep].safe_name, ');')
    end
    write(nl, 'const run_tests', i, ' = b.addRunArtifact(tests', i, ');', nl)
    i = i + 1
end

if i > 1 then
    nl()
    write 'const test_step = b.step("test", "Run all tests");'

    local i = 1
    for test in spairs(tests) do
        write(nl, 'test_step.dependOn(&run_tests', i, '.step);')
        i = i + 1
    end

    nl()
end

for _, module in spairs(modules) do
    if not module.used then
        write(nl, '_ = ', module.safe_name, ';')
    end
end
