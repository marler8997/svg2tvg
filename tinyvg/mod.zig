// placeholder until tvg updates to 0.13.0
const std = @import("std");

/// A TinyVG scale value. Defines the scale for all units inside a graphic.
/// The scale is defined by the number of decimal bits in a `i32`, thus scaling
/// can be trivially implemented by shifting the integers right by the scale bits.
pub const Scale = enum(u4) {
    const Self = @This();

    @"1/1" = 0,
    @"1/2" = 1,
    @"1/4" = 2,
    @"1/8" = 3,
    @"1/16" = 4,
    @"1/32" = 5,
    @"1/64" = 6,
    @"1/128" = 7,
    @"1/256" = 8,
    @"1/512" = 9,
    @"1/1024" = 10,
    @"1/2048" = 11,
    @"1/4096" = 12,
    @"1/8192" = 13,
    @"1/16384" = 14,
    @"1/32768" = 15,

    pub fn map(self: *const Self, value: f64) Unit {
        return Unit.init64(self.*, value);
    }

    pub fn getShiftBits(self: *const Self) u4 {
        return @intFromEnum(self.*);
    }

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // Original is using u15 instead of u16?
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    pub fn getScaleFactor(self: *const Self) u16 {
        return @as(u16, 1) << self.getShiftBits();
    }
};
pub const Unit = enum(i32) {
    const Self = @This();

    _,

    pub fn init(scale: Scale, value: f32) Self {
        return @enumFromInt(@as(i32, @intFromFloat(value * @as(f32, @floatFromInt(scale.getScaleFactor())) + 0.5)));
    }
    pub fn init64(scale: Scale, value: f64) Self {
        return @enumFromInt(@as(i32, @intFromFloat(value * @as(f64, @floatFromInt(scale.getScaleFactor())) + 0.5)));
    }

    pub fn raw(self: *const Self) i32 {
        return @intFromEnum(self.*);
    }

    pub fn toFloat(self: *const Self, scale: Scale) f32 {
        return @as(f32, @floatFromInt(@intFromEnum(self.*))) / @as(f32, @floatFromInt(scale.getScaleFactor()));
    }

    pub fn toInt(self: *const Self, scale: Scale) i32 {
        const factor = scale.getScaleFactor();
        return @divFloor(@intFromEnum(self.*) + (@divExact(factor, 2)), factor);
    }

    pub fn toUnsignedInt(self: *const Self, scale: Scale) !u31 {
        const i = toInt(self, scale);
        if (i < 0)
            return error.InvalidData;
        return @intCast(i);
    }
};

/// The value range used in the encoding.
pub const Range = enum(u2) {
    /// unit uses 16 bit,
    default = 0,

    /// unit takes only 8 bit
    reduced = 1,

    // unit uses 32 bit,
    enhanced = 2,
};

pub const Color = extern struct {
    const Self = @This();

    r: f32,
    g: f32,
    b: f32,
    a: f32,

    pub fn toRgba8(self: *const Self) [4]u8 {
        return [4]u8{
            @intFromFloat(std.math.clamp(255.0 * self.r, 0.0, 255.0)),
            @intFromFloat(std.math.clamp(255.0 * self.g, 0.0, 255.0)),
            @intFromFloat(std.math.clamp(255.0 * self.b, 0.0, 255.0)),
            @intFromFloat(std.math.clamp(255.0 * self.a, 0.0, 255.0)),
        };
    }

    pub fn lerp(lhs: Self, rhs: Self, factor: f32) Self {
        const l = struct {
            fn l(a: f32, b: f32, c: f32) u8 {
                return @intFromFloat(@as(f32, @floatFromInt(a)) + (@as(f32, @floatFromInt(b)) - @as(f32, @floatFromInt(a))) * std.math.clamp(c, 0, 1));
            }
        }.l;

        return Self{
            .r = l(lhs.r, rhs.r, factor),
            .g = l(lhs.g, rhs.g, factor),
            .b = l(lhs.b, rhs.b, factor),
            .a = l(lhs.a, rhs.a, factor),
        };
    }

    pub fn fromString(str: []const u8) !Self {
        return switch (str.len) {
            3 => Self{
                .r = @as(f32, @floatFromInt(try parseHexSingle(str[0]))) / 255.0,
                .g = @as(f32, @floatFromInt(try parseHexSingle(str[1]))) / 255.0,
                .b = @as(f32, @floatFromInt(try parseHexSingle(str[2]))) / 255.0,
                .a = 1.0,
            },
            6 => Self{
                .r = @as(f32, @floatFromInt(try std.fmt.parseInt(u8, str[0..2], 16))) / 255.0,
                .g = @as(f32, @floatFromInt(try std.fmt.parseInt(u8, str[2..4], 16))) / 255.0,
                .b = @as(f32, @floatFromInt(try std.fmt.parseInt(u8, str[4..6], 16))) / 255.0,
                .a = 1.0,
            },
            else => error.InvalidFormat,
        };
    }
};

fn parseHexSingle(c: u8) !u8 {
    switch (c) {
        '0'...'9' => |v| {
            const val: u8 = (v - '0');
            return val << 4 | val;
        },
        'A'...'F' => |v| {
            const val: u8 = (v - ('A' - 10));
            return val << 4 | val;
        },
        'a'...'f' => |v| {
            const val: u8 = (v - ('a' - 10));
            return val << 4 | val;
        },
        else => return error.InvalidCharacter,
    }
}
