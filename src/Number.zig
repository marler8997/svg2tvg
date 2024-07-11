/// A fixed-point lossless Number representation for floating point values
/// that are given as decimal strings.
const Number = @This();

const std = @import("std");
const testing = std.testing;

bits: i32,
frac_digits: u6,

pub fn getBitsMultipler(self: Number) i64 {
    return std.math.pow(i64, 10, self.frac_digits);
}
pub fn toFloat(self: Number, comptime Float: type) Float {
    const float: f64 = @floatFromInt(self.bits);
    const frac_digits: f64 = @floatFromInt(self.frac_digits);
    return @floatCast(float / std.math.pow(f64, 10, frac_digits));
}
pub fn negate(self: Number) Number {
    return .{ .bits = -self.bits, .frac_digits = self.frac_digits };
}
pub fn abs(self: Number) Number {
    return .{ .bits = @intCast(@abs(self.bits)), .frac_digits = self.frac_digits };
}
pub fn equalsInteger(self: Number, int: anytype) bool {
    const int_i32 = std.math.cast(i32, int) orelse return false;
    const scale = std.math.powi(i32, 10, self.frac_digits) catch return false;
    const scaled_int = @mulWithOverflow(int_i32, scale);
    if (scaled_int[1] != 0) return false;
    return self.bits == scaled_int[0];
}
pub fn order(self: Number, other: Number) std.math.Order {
    const self_float = self.toFloat(f64);
    const other_float = other.toFloat(f64);
    if (self_float < other_float)
        return .lt;
    if (self_float > other_float)
        return .gt;
    std.debug.assert(self_float == other_float);
    return .eq;
}
pub fn add(self: Number, other: Number) Number {
    return addOverflow(self, other) orelse std.debug.panic(
        "overflow adding {} and {}",
        .{ self, other}
    );
}
pub fn addOverflow(self: Number, other: Number) ?Number {
    const max_frac_digits = @max(self.frac_digits, other.frac_digits);
    const scale_self = std.math.powi(i64, 10, max_frac_digits - self.frac_digits) catch return null;
    const scale_other = std.math.powi(i64, 10, max_frac_digits - other.frac_digits) catch return null;
    const self_bits = @mulWithOverflow(self.bits, scale_self);
    if (self_bits[1] != 0) return null;
    const other_bits = @mulWithOverflow(other.bits, scale_other);
    if (other_bits[1] != 0) return null;
    const sum = @addWithOverflow(
        std.math.cast(i32, self.bits * scale_self) orelse return null,
        std.math.cast(i32, other.bits * scale_other) orelse return null,
    );
    if (sum[1] != 0) return null;
    return Number{
        .bits = sum[0],
        .frac_digits = max_frac_digits,
    };
}
pub fn format(
    self: Number,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    if (self.frac_digits == 0) {
        try writer.print("{d}", .{self.bits});
    } else if (self.bits == 0) {
        const end: usize = @as(usize, self.frac_digits) + 2;
        var buf: [100]u8 = undefined;
        buf[0] = '0';
        buf[1] = '.';
        @memset(buf[2..end], '0');
        try writer.writeAll(buf[0..end]);
    } else {
        var buf: [100]u8 = undefined;
        const str = blk: {
            const unsigned: u32 = @intCast(if (self.bits >= 0) self.bits else -@as(i33, self.bits));
            const str = std.fmt.bufPrint(&buf, "{d}", .{unsigned}) catch unreachable;
            if (str.len >= self.frac_digits) break :blk str;
            const diff = self.frac_digits - str.len;
            std.mem.copyForwards(u8, buf[diff..][0..str.len], str);
            @memset(buf[0..diff], '0');
            break :blk buf[0 .. self.frac_digits];
        };
        const dot: usize = str.len - self.frac_digits;
        try writer.print("{s}{s}.{s}", .{
            @as([]const u8, if (self.bits >= 0) "" else "-"),
            str[0..dot],
            str[dot..]
        });
    }
}

pub fn parse(s: []const u8) error{Overflow,EmptyInput,NotANumber}!Number {
    const result = (try parseLazy(s)) orelse {
        if (s.len == 0) return error.EmptyInput;
        return error.NotANumber;
    };
    return result.num;
}

pub fn parseLazy(s: []const u8) error{Overflow}!?struct {
    char_count: usize,
    num: Number
} {
    if (s.len == 0) return null;
    var index: usize = 0;
    const is_negative = blk: {
        if (s[index] == '-') {
            index += 1;
            if (index == s.len)
                return null;
            break :blk true;
        }
        break :blk false;
    };
    var value: u32 = blk: {
        const result = try parseDigits(s[index..], 0);
        index += result.char_count;
        break :blk result.new_value;
    };
    var frac_digits: u6 = 0;
    if (index < s.len and s[index] == '.') {
        index += 1;
        const result = try parseDigits(s[index..], value);
        index += result.char_count;
        frac_digits = std.math.cast(u6, result.char_count) orelse return error.Overflow;
        value = result.new_value;
    }
    if (index < s.len and (s[index] == 'e' or s[index] == 'E'))
        @panic("TODO: scientific notation");
    if (index == 0)
        return null;
    const value_i32 = blk: {
        if (is_negative) break :blk std.math.cast(
            i32, @as(i33, value) * -1
        ) orelse return error.Overflow;
        break :blk std.math.cast(
            i32, value
        ) orelse return error.Overflow;
    };
    return .{
        .char_count = index,
        .num = .{
            .bits = value_i32,
            .frac_digits = frac_digits,
        },
    };
}

fn parseDigits(s: []const u8, init: u32) error{Overflow}!struct{
    char_count: usize,
    new_value: u32,
} {
    var num = init;
    var index: usize = 0;
    while (true) : (index += 1) {
        if (index == s.len or s[index] > '9' or s[index] < '0')
            break;
        {
            const result = @mulWithOverflow(num, 10);
            if (result[1] != 0)
                return error.Overflow;
            num = result[0];
        }
        {
            const result = @addWithOverflow(num, s[index]-'0');
            if (result[1] != 0)
                return error.Overflow;
            num = result[0];
        }
    }
    return .{ .char_count = index, .new_value = num };
}

test equalsInteger {
    try testing.expect((Number{ .bits = 0, .frac_digits = 0}).equalsInteger(0));
    try testing.expect((Number{ .bits = 1, .frac_digits = 0}).equalsInteger(1));
    try testing.expect((Number{ .bits = 10, .frac_digits = 1}).equalsInteger(1));
    try testing.expect((Number{ .bits = 100, .frac_digits = 2}).equalsInteger(1));
    try testing.expect((Number{ .bits = 255, .frac_digits = 0}).equalsInteger(255));
    try testing.expect((Number{ .bits = 2550, .frac_digits = 1}).equalsInteger(255));
    try testing.expect((Number{ .bits = 2550000, .frac_digits = 4}).equalsInteger(255));
    try testing.expect((Number{ .bits = -1, .frac_digits = 0}).equalsInteger(-1));
    try testing.expect((Number{ .bits = -10, .frac_digits = 1}).equalsInteger(-1));
    try testing.expect((Number{ .bits = -128, .frac_digits = 0}).equalsInteger(-128));
}

test parse {
    try testing.expectError(error.EmptyInput, parse(""));
    try testing.expectError(error.NotANumber, parse("-"));
    try testing.expectEqual(Number{ .bits = 0, .frac_digits = 0}, parse("0"));
    try testing.expectEqual(Number{ .bits = 0, .frac_digits = 0}, parse("-0"));
    try testing.expectEqual(Number{ .bits = 0, .frac_digits = 1}, parse("0.0"));
    try testing.expectEqual(Number{ .bits = 0, .frac_digits = 1}, parse("-0.0"));
    try testing.expectEqual(Number{ .bits = 1, .frac_digits = 0}, parse("1"));
    try testing.expectEqual(Number{ .bits = -1, .frac_digits = 0}, parse("-1"));
    try testing.expectEqual(Number{ .bits = 10, .frac_digits = 1}, parse("1.0"));
    try testing.expectEqual(Number{ .bits = -10, .frac_digits = 1}, parse("-1.0"));
    try testing.expectEqual(Number{ .bits = -123456, .frac_digits = 3}, parse("-123.456"));
    try testing.expectEqual(Number{ .bits = -987654321, .frac_digits = 9}, parse("-.987654321"));
}
test "Number values" {
    try testing.expectEqual(@as(f64, 0), (try parse("0")).toFloat());
    try testing.expectEqual(@as(f64, 0), (try parse("-0")).toFloat());
    try testing.expectEqual(@as(f64, 0), (try parse("0.0")).toFloat());
    try testing.expectEqual(@as(f64, 0), (try parse("-0.0")).toFloat());
    try testing.expectEqual(@as(f64, 0), (try parse("0.00000000000000")).toFloat());
    const max_zero = (
        "-0.000000000000000000000000000000000000000000000000000000000000000"
    );
    try testing.expectEqual(@as(f64, 0), (try parse(max_zero)).toFloat());
    try testing.expectError(error.Overflow, parse(max_zero ++ "0"));
    try testing.expectEqual(@as(f64, 999999999), (try parse("999999999")).toFloat());
    try testing.expectError(error.Overflow, parse("9999999999"));
    try testing.expectError(error.Overflow, parse("99999.99999"));
    try testing.expectError(error.Overflow, parse(".9999999999"));
    try testing.expectEqual(@as(f64, 1), (try parse("1.")).toFloat());
    try testing.expectEqual(@as(f64, 1), (try parse("1.0")).toFloat());
    try testing.expectEqual(@as(f64, 1), (try parse("1.00")).toFloat());
}

test add {
    const extremes = [_]i32{
        0, 1, 2,
        0xff, 0xffff, 0xffffff,
        0x7ffffffe,
        0x7fffffff,
        -1, -2,
        -0xff, -0xffff, -0xffffff,
        -0x7ffffffe,
        -0x7fffffff,
        -0x80000000,
    };
    const zero: Number = .{ .bits = 0, .frac_digits = 0 };
    for (0 .. 63) |frac_digits| {
        for (extremes) |extreme_i32| {
            const extreme_num: Number = .{ .bits = extreme_i32, .frac_digits = @intCast(frac_digits) };
            var extreme_str_buf: [100]u8 = undefined;
            const extreme_str = try std.fmt.bufPrint(&extreme_str_buf, "{}", .{extreme_num});

            if (frac_digits <= 18) {
                const sum = extreme_num.add(zero);
                try testing.expectEqual(extreme_num, sum);
                var sum_str_buf: [100]u8 = undefined;
                const sum_str = try std.fmt.bufPrint(&sum_str_buf, "{}", .{sum});
                try testing.expectEqualSlices(u8, extreme_str, sum_str);
            }

            for (extremes) |other_extreme_i32| {
                if (frac_digits != 0) continue;
                const other_extreme_num: Number = .{ .bits = other_extreme_i32, .frac_digits = 0 };
                const sum = @addWithOverflow(extreme_i32, other_extreme_i32);
                if (sum[1] == 0) {
                    try testing.expectEqual(
                        Number{ .bits = sum[0], .frac_digits = 0 },
                        extreme_num.add(other_extreme_num),
                    );
                }
            }
        }
    }
}
