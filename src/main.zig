const std = @import("std");
const zbench = @import("zbench");

pub const Bench_indexOfDiff_V1 = struct {
    a: []const u8,
    b: []const u8,

    fn init(allocator: std.mem.Allocator, len: usize) !Bench_indexOfDiff_V1 {
        const a = try allocator.alloc(u8, len * 2);
        const b = try allocator.alloc(u8, len * 2);

        var prng = std.Random.DefaultPrng.init(@intCast(std.time.timestamp()));
        const rand = prng.random();

        for (a[0..len], b[0..len]) |*x, *y| {
            const val = rand.int(u8);
            x.* = val;
            y.* = val;
        }

        for (a[len..], b[len..]) |*x, *y| {
            x.* = rand.int(u8);
            y.* = rand.int(u8);
        }

        // std.debug.print("v1: {?}\n", .{indexOfDiff_V1(u8, a, b)});

        return .{ .a = a, .b = b };
    }

    pub fn run(self: Bench_indexOfDiff_V1, _: std.mem.Allocator) void {
        const index = indexOfDiff_V1(u8, self.a, self.b);
        std.mem.doNotOptimizeAway(index);
    }
};

pub const Bench_indexOfDiff_V2 = struct {
    a: []const u8,
    b: []const u8,

    fn init(allocator: std.mem.Allocator, len: usize) !Bench_indexOfDiff_V2 {
        const a = try allocator.alloc(u8, len * 2);
        const b = try allocator.alloc(u8, len * 2);

        var prng = std.Random.DefaultPrng.init(@intCast(std.time.timestamp()));
        const rand = prng.random();

        for (a[0..len], b[0..len]) |*x, *y| {
            const val = rand.int(u8);
            x.* = val;
            y.* = val;
        }

        for (a[len..], b[len..]) |*x, *y| {
            x.* = rand.int(u8);
            y.* = rand.int(u8);
        }

        // std.debug.print("v2: {?}\n", .{indexOfDiff_V2(u8, a, b)});

        return .{ .a = a, .b = b };
    }

    pub fn run(self: Bench_indexOfDiff_V2, _: std.mem.Allocator) void {
        const index = indexOfDiff_V2(u8, self.a, self.b);
        std.mem.doNotOptimizeAway(index);
    }
};

pub const Bench_eql = struct {
    a: []const u8,
    b: []const u8,

    fn init(allocator: std.mem.Allocator, len: usize) !Bench_eql {
        const a = try allocator.alloc(u8, len * 2);
        const b = try allocator.alloc(u8, len * 2);

        var prng = std.Random.DefaultPrng.init(@intCast(std.time.timestamp()));
        const rand = prng.random();

        for (a[0..len], b[0..len]) |*x, *y| {
            const val = rand.int(u8);
            x.* = val;
            y.* = val;
        }

        for (a[len..], b[len..]) |*x, *y| {
            x.* = rand.int(u8);
            y.* = rand.int(u8);
        }

        // std.debug.print("v2: {?}\n", .{indexOfDiff_V2(u8, a, b)});

        return .{ .a = a, .b = b };
    }

    pub fn run(self: Bench_eql, _: std.mem.Allocator) void {
        const index = std.mem.eql(u8, self.a, self.b);
        std.mem.doNotOptimizeAway(index);
    }
};

noinline fn indexOfDiff_V1(comptime T: type, a: []const T, b: []const T) ?usize {
    const shortest = @min(a.len, b.len);
    if (a.ptr == b.ptr)
        return if (a.len == b.len) null else shortest;
    var index: usize = 0;
    while (index < shortest) : (index += 1) if (a[index] != b[index]) return index;
    return if (a.len == b.len) null else shortest;
}

fn CopyPtrAttrs(
    comptime source: type,
    comptime size: std.builtin.Type.Pointer.Size,
    comptime child: type,
) type {
    const info = @typeInfo(source).pointer;
    return @Type(.{
        .pointer = .{
            .size = size,
            .is_const = info.is_const,
            .is_volatile = info.is_volatile,
            .is_allowzero = info.is_allowzero,
            .alignment = info.alignment,
            .address_space = info.address_space,
            .child = child,
            .sentinel_ptr = null,
        },
    });
}

fn SliceAsBytesReturnType(comptime Slice: type) type {
    return CopyPtrAttrs(Slice, .slice, u8);
}

pub fn sliceAsBytes(slice: anytype) SliceAsBytesReturnType(@TypeOf(slice)) {
    const Slice = @TypeOf(slice);

    // a slice of zero-bit values always occupies zero bytes
    if (@sizeOf(std.meta.Elem(Slice)) == 0) return &[0]u8{};

    // let's not give an undefined pointer to @ptrCast
    // it may be equal to zero and fail a null check
    if (slice.len == 0 and std.meta.sentinel(Slice) == null) return &[0]u8{};

    const cast_target = CopyPtrAttrs(Slice, .many, u8);

    return @as(cast_target, @ptrCast(slice))[0 .. slice.len * @sizeOf(std.meta.Elem(Slice))];
}

const native_endian = @import("builtin").cpu.arch.endian();

noinline fn indexOfDiff_V2(comptime T: type, a: []const T, b: []const T) ?usize {
    if (!@inComptime() and @sizeOf(T) != 0 and std.meta.hasUniqueRepresentation(T))
        return if (indexOfDiffBytes_V2(sliceAsBytes(a), sliceAsBytes(b))) |index| index / @sizeOf(T) else return null;

    const shortest = @min(a.len, b.len);
    if (a.ptr == b.ptr) return if (a.len == b.len) null else shortest;
    var index: usize = 0;
    while (index < shortest) : (index += 1) if (a[index] != b[index]) return index;
    return if (a.len == b.len) null else shortest;
}

/// std.mem.indexOfDiff heavily optimized for slices of bytes.
fn indexOfDiffBytes_V2(a: []const u8, b: []const u8) ?usize {
    const shortest = @min(a.len, b.len);
    const vec_len = std.simd.suggestVectorLength(u8) orelse 0;
    if (a.ptr == b.ptr) return if (a.len == b.len) null else shortest;

    if (shortest < @sizeOf(usize)) {
        for (0..shortest) |index| if (a[index] != b[index]) return index;
    }
    // Use SWAR when the slice is small or SIMD is not supported
    else if (shortest < 16 or vec_len == 0) {
        var index: usize = 0;
        while (index + @sizeOf(usize) <= shortest) : (index += @sizeOf(usize)) {
            const a_chunk: usize = @bitCast(a[index..][0..@sizeOf(usize)].*);
            const b_chunk: usize = @bitCast(b[index..][0..@sizeOf(usize)].*);
            const diff = a_chunk ^ b_chunk;
            if (diff != 0) {
                const offset = @divFloor(if (native_endian == .little) @ctz(diff) else @clz(diff), 8);
                return index + offset;
            }
        }
        if (index < shortest) {
            const a_chunk: usize = @bitCast(a[shortest - @sizeOf(usize) ..][0..@sizeOf(usize)].*);
            const b_chunk: usize = @bitCast(b[shortest - @sizeOf(usize) ..][0..@sizeOf(usize)].*);
            const diff = a_chunk ^ b_chunk;
            if (diff != 0) {
                const offset = @divFloor(if (native_endian == .little) @ctz(diff) else @clz(diff), 8);
                return shortest - @sizeOf(usize) + offset;
            }
        }
    }
    // When the slice is smaller than the max vector length, reselect an appropriate vector length.
    else if (shortest < vec_len) {
        comptime var new_vec_len = 16;
        inline while (new_vec_len < vec_len) : (new_vec_len *= 2) {
            if (new_vec_len < shortest and 2 * new_vec_len >= shortest) {
                inline for ([_]usize{ 0, shortest - new_vec_len }) |index| {
                    const a_chunk: @Vector(new_vec_len, u8) = @bitCast(a[index..][0..new_vec_len].*);
                    const b_chunk: @Vector(new_vec_len, u8) = @bitCast(b[index..][0..new_vec_len].*);
                    const diff = a_chunk != b_chunk;
                    if (@reduce(.Or, diff)) return index + std.simd.firstTrue(diff).?;
                }
                break;
            }
        }
    }
    // Using max vector length to perform SIMD scanning on slice
    else {
        var index: usize = 0;
        const unroll_factor = 4;
        while (index + vec_len * unroll_factor <= shortest) : (index += vec_len * unroll_factor) {
            inline for (0..unroll_factor) |i| {
                const a_chunk: @Vector(vec_len, u8) = @bitCast(a[index + vec_len * i ..][0..vec_len].*);
                const b_chunk: @Vector(vec_len, u8) = @bitCast(b[index + vec_len * i ..][0..vec_len].*);
                const diff = a_chunk != b_chunk;
                if (@reduce(.Or, diff)) return index + vec_len * i + std.simd.firstTrue(diff).?;
            }
        }
        while (index + vec_len <= shortest) : (index += vec_len) {
            const a_chunk: @Vector(vec_len, u8) = @bitCast(a[index..][0..vec_len].*);
            const b_chunk: @Vector(vec_len, u8) = @bitCast(b[index..][0..vec_len].*);
            const diff = a_chunk != b_chunk;
            if (@reduce(.Or, diff)) return index + std.simd.firstTrue(diff).?;
        }

        if (index < shortest) {
            const a_chunk: @Vector(vec_len, u8) = @bitCast(a[shortest - vec_len ..][0..vec_len].*);
            const b_chunk: @Vector(vec_len, u8) = @bitCast(b[shortest - vec_len ..][0..vec_len].*);
            const diff = a_chunk != b_chunk;
            if (@reduce(.Or, diff)) return shortest - vec_len + std.simd.firstTrue(diff).?;
        }
    }

    return if (a.len == b.len) null else shortest;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var bench = zbench.Benchmark.init(allocator, .{});
    defer bench.deinit();

    inline for ([_]usize{ 2, 5, 10, 20, 50, 100, 1000, 10000, 100000 }) |len| {
        const bench_indexOfDiff_V1 = try Bench_indexOfDiff_V1.init(allocator, len);
        defer allocator.free(bench_indexOfDiff_V1.a);
        defer allocator.free(bench_indexOfDiff_V1.b);

        const bench_indexOfDiff_V2 = try Bench_indexOfDiff_V2.init(allocator, len);
        defer allocator.free(bench_indexOfDiff_V2.a);
        defer allocator.free(bench_indexOfDiff_V2.b);

        const bench_eql = try Bench_eql.init(allocator, len);
        defer allocator.free(bench_eql.a);
        defer allocator.free(bench_eql.b);

        try bench.addParam(std.fmt.comptimePrint("indexOfDiff(V1)/{}", .{len}), &bench_indexOfDiff_V1, .{});
        try bench.addParam(std.fmt.comptimePrint("indexOfDiff(V2)/{}", .{len}), &bench_indexOfDiff_V2, .{});
        try bench.addParam(std.fmt.comptimePrint("eql/{}", .{len}), &bench_eql, .{});
    }

    try stdout.writeAll("\n");
    try bench.run(stdout);
}
