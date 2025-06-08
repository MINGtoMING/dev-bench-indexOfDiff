const std = @import("std");

pub fn main() !void {
    const num_run = 10_000;
    const type_list = [_]type{ u8, u32, u128 };
    const step_list = [_]usize{ 5, 10, 20, 50, 100, 1000, 10000 };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var prng = std.Random.DefaultPrng.init(@intCast(std.time.timestamp()));
    const rand = prng.random();

    std.debug.print("{s:^30} {s:^8}\n", .{ "fn/T/len", "elapsed" });

    inline for (type_list) |T| {
        for (step_list) |step| {
            std.debug.print("{s}\n", .{"-" ** 39});

            const len = step * num_run;

            const a = try allocator.alloc(T, len);
            defer allocator.free(a);

            const b = try allocator.alloc(T, len);
            defer allocator.free(b);

            std.mem.doNotOptimizeAway(a);
            std.mem.doNotOptimizeAway(b);

            // Init data
            {
                @memset(a, rand.int(T));
                @memcpy(b, a);
                var i: usize = 0;
                while (i + step <= len) : (i += step) {
                    a[i + step - 1] = rand.int(T);
                    b[i + step - 1] = a[i + step - 1] +% 1;
                }
            }

            // Warm up `indexOfDiff_V1`
            {
                var i: usize = 0;
                while (i + step <= len) : (i += step) {
                    const idx = indexOfDiff_V1(T, a[i..][0..step], b[i..][0..step]);
                    std.debug.assert(idx == step - 1);
                }
            }

            // Bench `indexOfDiff_V1`
            {
                var timer = try std.time.Timer.start();
                timer.reset();

                var tmp: usize = 0;
                var i: usize = 0;
                while (i + step <= len) : (i += step) {
                    tmp += indexOfDiff_V1(T, a[i..][0..step], b[i..][0..step]) orelse 0;
                }
                std.mem.doNotOptimizeAway(tmp);

                const elapsed = timer.lap() / num_run;
                const info = try std.fmt.allocPrint(allocator, "{s}/{}/{}", .{ "indexOfDiff_V1", T, step });
                defer allocator.free(info);
                std.debug.print("{s:^30} {:^8}\n", .{ info, std.fmt.fmtDuration(elapsed) });
            }

            // Warm up `indexOfDiff_V2`
            {
                var i: usize = 0;
                while (i + step <= len) : (i += step) {
                    const idx = indexOfDiff_V2(T, a[i..][0..step], b[i..][0..step]);
                    std.debug.assert(idx == step - 1);
                }
            }

            // Bench `indexOfDiff_V2`
            {
                var timer = try std.time.Timer.start();
                timer.reset();

                var tmp: usize = 0;
                var i: usize = 0;
                while (i + step <= len) : (i += step) {
                    tmp += indexOfDiff_V2(T, a[i..][0..step], b[i..][0..step]) orelse 0;
                }
                std.mem.doNotOptimizeAway(tmp);

                const elapsed = timer.lap() / num_run;
                const info = try std.fmt.allocPrint(allocator, "{s}/{}/{}", .{ "indexOfDiff_V2", T, step });
                defer allocator.free(info);
                std.debug.print("{s:^30} {:^8}\n", .{ info, std.fmt.fmtDuration(elapsed) });
            }

            // Warm up `std.mem.eql`
            {
                var i: usize = 0;
                while (i + step <= len) : (i += step) {
                    std.debug.assert(std.mem.eql(T, a[i..][0..step], b[i..][0..step]) == false);
                }
            }

            // Bench `std.mem.eql`
            {
                var timer = try std.time.Timer.start();
                timer.reset();

                var tmp: usize = 0;
                var i: usize = 0;
                while (i + step <= len) : (i += step) {
                    tmp += @intFromBool(std.mem.eql(T, a[i..][0..step], b[i..][0..step]));
                }
                std.mem.doNotOptimizeAway(tmp);

                const elapsed = timer.lap() / num_run;
                const info = try std.fmt.allocPrint(allocator, "{s}/{}/{}", .{ "std.mem.eql", T, step });
                defer allocator.free(info);
                std.debug.print("{s:^30} {:^8}\n", .{ info, std.fmt.fmtDuration(elapsed) });
            }
        }
    }
}

fn indexOfDiff_V1(comptime T: type, a: []const T, b: []const T) ?usize {
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

fn indexOfDiff_V2(comptime T: type, a: []const T, b: []const T) ?usize {
    if (!@inComptime() and @sizeOf(T) != 0 and std.meta.hasUniqueRepresentation(T))
        return if (indexOfDiffBytes_V2(sliceAsBytes(a), sliceAsBytes(b))) |index| index / @sizeOf(T) else null;

    const shortest = @min(a.len, b.len);
    if (a.ptr == b.ptr) return if (a.len == b.len) null else shortest;
    var index: usize = 0;
    while (index < shortest) : (index += 1) if (a[index] != b[index]) return index;
    return if (a.len == b.len) null else shortest;
}

/// std.mem.indexOfDiff heavily optimized for slices of bytes.
fn indexOfDiffBytes_V2(a: []const u8, b: []const u8) ?usize {
    const shortest = @min(a.len, b.len);
    if (a.ptr == b.ptr) return if (a.len == b.len) null else shortest;

    if (shortest < 16) {
        if (shortest < @sizeOf(usize)) {
            for (0..shortest) |index| if (a[index] != b[index]) return index;
        } else {
            var index: usize = 0;
            while (index + @sizeOf(usize) <= shortest) : (index += @sizeOf(usize)) {
                const a_chunk: usize = @bitCast(a[index..][0..@sizeOf(usize)].*);
                const b_chunk: usize = @bitCast(b[index..][0..@sizeOf(usize)].*);
                const diff = a_chunk ^ b_chunk;
                if (diff != 0)
                    return index + @divFloor(if (native_endian == .little) @ctz(diff) else @clz(diff), 8);
            }
            if (index < shortest) {
                const a_chunk: usize = @bitCast(a[shortest - @sizeOf(usize) ..][0..@sizeOf(usize)].*);
                const b_chunk: usize = @bitCast(b[shortest - @sizeOf(usize) ..][0..@sizeOf(usize)].*);
                const diff = a_chunk ^ b_chunk;
                if (diff != 0)
                    return shortest - @sizeOf(usize) + @divFloor(if (native_endian == .little) @ctz(diff) else @clz(diff), 8);
            }
        }
        return if (a.len == b.len) null else shortest;
    }

    const Scan = if (std.simd.suggestVectorLength(u8)) |vec_len| struct {
        const size = vec_len;

        pub inline fn isNotZero(cur_size: comptime_int, mask: @Vector(cur_size, bool)) bool {
            return @reduce(.Or, mask);
        }

        pub inline fn firstTrue(cur_size: comptime_int, mask: @Vector(cur_size, bool)) usize {
            return std.simd.firstTrue(mask).?;
        }
    } else struct {
        const size = @sizeOf(usize);

        pub inline fn isNotZero(_: comptime_int, mask: usize) bool {
            return mask != 0;
        }
        pub inline fn firstTrue(_: comptime_int, mask: usize) usize {
            return @divFloor(if (native_endian == .little) @ctz(mask) else @clz(mask), 8);
        }
    };

    // When the slice is smaller than the max vector length, reselect an appropriate vector length.
    if (shortest < Scan.size) {
        comptime var new_vec_len = 16;
        inline while (new_vec_len < Scan.size) : (new_vec_len *= 2) {
            if (new_vec_len < shortest and 2 * new_vec_len >= shortest) {
                inline for ([_]usize{ 0, shortest - new_vec_len }) |index| {
                    const a_chunk: @Vector(new_vec_len, u8) = @bitCast(a[index..][0..new_vec_len].*);
                    const b_chunk: @Vector(new_vec_len, u8) = @bitCast(b[index..][0..new_vec_len].*);
                    const diff = a_chunk != b_chunk;
                    if (Scan.isNotZero(new_vec_len, diff))
                        return index + Scan.firstTrue(new_vec_len, diff);
                }
                break;
            }
        }
    }
    // Using max vector length to perform SIMD scanning on slice
    else {
        var index: usize = 0;
        const unroll_factor = 4;
        while (index + Scan.size * unroll_factor <= shortest) : (index += Scan.size * unroll_factor) {
            inline for (0..unroll_factor) |i| {
                const a_chunk: @Vector(Scan.size, u8) = @bitCast(a[index + Scan.size * i ..][0..Scan.size].*);
                const b_chunk: @Vector(Scan.size, u8) = @bitCast(b[index + Scan.size * i ..][0..Scan.size].*);
                const diff = a_chunk != b_chunk;
                if (Scan.isNotZero(Scan.size, diff))
                    return index + Scan.size * i + Scan.firstTrue(Scan.size, diff);
            }
        }
        while (index + Scan.size <= shortest) : (index += Scan.size) {
            const a_chunk: @Vector(Scan.size, u8) = @bitCast(a[index..][0..Scan.size].*);
            const b_chunk: @Vector(Scan.size, u8) = @bitCast(b[index..][0..Scan.size].*);
            const diff = a_chunk != b_chunk;
            if (Scan.isNotZero(Scan.size, diff))
                return index + Scan.firstTrue(Scan.size, diff);
        }

        if (index < shortest) {
            const a_chunk: @Vector(Scan.size, u8) = @bitCast(a[shortest - Scan.size ..][0..Scan.size].*);
            const b_chunk: @Vector(Scan.size, u8) = @bitCast(b[shortest - Scan.size ..][0..Scan.size].*);
            const diff = a_chunk != b_chunk;
            if (Scan.isNotZero(Scan.size, diff))
                return shortest - Scan.size + Scan.firstTrue(Scan.size, diff);
        }
    }

    return if (a.len == b.len) null else shortest;
}
