-- This file is used to generate build.zig, based on the configuration in build.sx

local parser = sx.parser(get_file_contents('build.sx'))
local root_visitor = {}
local dir_visitor = {}
local pkg_visitor = {}
local exe_visitor = {}

local dirs = {}
local notest_dirs = {}
local file_deps = {}
local packages = {}
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

function root_visitor.module ()
    local name = parser:require_string()
    local package = {
        name = name,
        safe_name = make_safe_name(name),
        module = true,
        extra_exe_config = {},
        pass_exe_to = {},
    }
    while nil ~= parser:property(pkg_visitor, package) do end
    parser:require_close()
    packages[package.name] = package
end

function dir_visitor._ ()
    -- Comment
    parser:ignore_remaining_expression()
end

function dir_visitor.pkg (_, _, dir_path)
    local name = parser:require_string()
    local default_path = fs.compose_path_slash(dir_path, fs.replace_extension(name, '.zig'))
    local package = {
        name = name,
        safe_name = make_safe_name(name),
        dir = dir_path,
        path = default_path,
        extra_exe_config = {},
        pass_exe_to = {},
    }
    while nil ~= parser:property(pkg_visitor, package) do end
    parser:require_close()
    packages[package.name] = package
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

function pkg_visitor.path (_, _, package)
    local filename = fs.replace_extension(parser:require_string(), '.zig')
    parser:require_close()
    package.path = fs.compose_path_slash(package.dir, filename)
end

function pkg_visitor.func (_, _, package)
    package.pass_exe_to[#package.pass_exe_to+1] = parser:require_string()
    parser:require_close()
end

function pkg_visitor.config (_, _, package)
    package.extra_exe_config[#package.extra_exe_config+1] = parser:require_string()
    parser:require_close()
end

function pkg_visitor.safe_name (_, _, package)
    local safe_name = parser:require_string()
    parser:require_close()
    package.safe_name = safe_name
end

function exe_visitor.run_step (_, _, executable)
    executable.run_step = parser:require_string()
    parser:require_close()
end

exe_visitor.func = pkg_visitor.func
exe_visitor.config = pkg_visitor.config
exe_visitor.safe_name = pkg_visitor.safe_name

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
            if packages[dep] == nil then
                error("Package not found: " .. dep)
            end
            named_deps[dep] = true
            packages[dep].used = true
        end
    end
end

-- Compile dependencies for each package
for _, package in pairs(packages) do
    if package.deps == nil then
        package.deps = {}
        if not package.module then
            collect_named_deps(package.path, package.deps, {})
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

local function get_package (pkg_name)
    local pkg = packages[pkg_name]
    if pkg == nil then
        error("Package " .. pkg_name .. " not found!")
    end
    return pkg
end

local function try_write_package (package)
    if package.module then return end

    if not package.written_to_build then
        if package.started_writing_to_build then
            if not package.written_to_build_without_dependencies then
                -- There's a circular dependency; we need a forward declaration to avoid infinite recursion
                write(nl, 'const ', package.safe_name, ' = b.createModule(.{', indent)
                write(nl, '.source_file = .{ .path = "', package.path, '" },')
                unindent()
                nl()
                write '});'
                nl()
                package.written_to_build_without_dependencies = true
            end
            return
        end

        package.started_writing_to_build = true
        for dep in spairs(package.deps) do
            try_write_package(get_package(dep))
        end
        if package.written_to_build_without_dependencies then
            if next(package.deps) then
                nl()
                for dep in spairs(package.deps) do
                    local safe_name = packages[dep].safe_name
                    writeln(package.safe_name, '.dependencies.put("', dep, '", ', safe_name, ') catch unreachable;')
                end
            end
        else
            write(nl, 'const ', package.safe_name, ' = b.createModule(.{', indent)
            write(nl, '.source_file = .{ .path = "', package.path, '" },')
            if next(package.deps) then
                nl()
                write '.dependencies = &.{'
                indent()
                for dep in spairs(package.deps) do
                    local safe_name = packages[dep].safe_name
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

        package.written_to_build = true
    end
end

for _, package in spairs(packages) do
    try_write_package(package)
end

local write_exe = template [[

const `safe_name` = b.addExecutable(.{
    .name = "`name`",
    .root_source_file = .{ .path = "`path`" },
    .target = target,
    .optimize = mode,
});`
local packages = ...
for dep in spairs(deps) do
    local pkg = packages[dep]
    write(nl, safe_name, '.addModule("', dep, '", ', pkg.safe_name,');')
    for _, fun in ipairs(pkg.extra_exe_config) do 
        write(nl, safe_name, '.', fun, ';')
    end
    for _, fun in ipairs(pkg.pass_exe_to) do
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
_ = makeRunStep(b, `safe_name`, "`run_step`", "run `name`");
]]

for _, exe in spairs(exes) do
    write_exe(exe, packages)
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
        write(nl, 'tests', i, '.addModule("', dep, '", ', packages[dep].safe_name, ');')
    end
    nl()
    i = i + 1
end

if i > 1 then
    nl()
    write 'const test_step = b.step("test", "Run all tests");'

    local i = 1
    for test in spairs(tests) do
        write(nl, 'test_step.dependOn(&tests', i, '.step);')
        i = i + 1
    end

    nl()
end

for _, package in spairs(packages) do
    if not package.used then
        write(nl, '_ = ', package.safe_name, ';')
    end
end