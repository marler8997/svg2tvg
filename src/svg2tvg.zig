const std = @import("std");
const testing = std.testing;
const xml = @import("xml");
const tvg = @import("tvg");
const Number = @import("Number.zig");

fn oom(e: error{OutOfMemory}) noreturn { @panic(@errorName(e)); }
fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.log.err(fmt, args);
    std.process.exit(0xff);
}

fn usage() !void {
    // TODO: add an option to specify the coorindate range (i.e. 8, 16 32 bit coorindates)
    try std.io.getStdErr().writer().writeAll(
        \\svg2tvg [--output OUT_FILE] SVG_FILE
        \\
        \\Converts an SVG file into the TinyVG text representation.
        \\Use tvg-text to convert output into binary.
        \\
        \\Options
        \\  -o, --output <file>   Writes the output tvgt to <file>. If not given, the output
        \\                        will be <input> with .tvgt extension.
        \\
    );
}

pub fn main() !u8 {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const arena = arena_instance.allocator();

    var cmd_opt: struct {
        output: ?[]const u8 = null,
    } = .{};

    const cmd_args = blk: {
        const all_args = try std.process.argsAlloc(arena);
        if (all_args.len <= 1) {
            try usage();
            return 0xff;
        }
        var new_arg_count: usize = 0;
        var arg_index: usize = 1;
        while (arg_index < all_args.len) : (arg_index += 1) {
            const arg = all_args[arg_index];
            if (!std.mem.startsWith(u8, arg, "-")) {
                all_args[new_arg_count] = arg;
                new_arg_count += 1;
            } else if (std.mem.eql(u8, arg, "--output") or std.mem.eql(u8, arg, "-o")) {
                arg_index += 1;
                if (arg_index >= all_args.len) fatal("--output requires an argument", .{});
                cmd_opt.output = all_args[arg_index];
            } else fatal("unknown cmdline option {s}", .{arg});
        }
        break :blk all_args[0 .. new_arg_count];
    };
    if (cmd_args.len != 1) fatal(
        "expected 1 non-option cmdline argument but got {}", .{cmd_args.len}
    );
    const svg_file_path = cmd_args[0];

    var file = std.fs.cwd().openFile(svg_file_path, .{}) catch |err| fatal(
        "open '{s}' failed with {s}", .{svg_file_path, @errorName(err)}
    );
    //defer file.close();
    const document = xml.parse(arena, "", file.reader()) catch |err| fatal(
        "failed to parse '{s}' as xml with {s}", .{svg_file_path, @errorName(err)}
    );
    document.acquire();

    const ext_index: usize = @intFromPtr(std.fs.path.extension(svg_file_path).ptr) - @intFromPtr(svg_file_path.ptr);
    const out_file_path = cmd_opt.output orelse
        try std.fmt.allocPrint(arena, "{s}.tvgt", .{svg_file_path[0 .. ext_index]});
    var out_file = std.fs.cwd().createFile(out_file_path, .{}) catch |err| fatal(
        "failed to create output file '{s}' with {s}", .{out_file_path, @errorName(err)}
    );
    defer out_file.close();
    var bw = std.io.bufferedWriter(out_file.writer());
    try writeTvgt(arena, bw.writer(), document);
    try bw.flush();
    return 0;
}

const RootAttr = enum {
    width,
    height,
    viewBox,
    fill,
    xmlns,
    xmlns_xlink,
    version,
};
const root_attr_map = std.StaticStringMap(RootAttr).initComptime(.{
    .{ "width", .width },
    .{ "height", .height },
    .{ "viewBox", .viewBox },
    .{ "fill", .fill },
    .{ "xmlns", .xmlns },
    .{ "xmlns:xlink", .xmlns_xlink },
    .{ "version", .version },
});

const InheritableAttributes = struct {
    opacity: Number = .{ .bits = 1, .frac_digits = 0 },
    fill: ?tvg.Color = black,
    @"fill-opacity": Number = .{ .bits = 1, .frac_digits = 0 },
    @"fill-rule": FillRule = FillRule.default(),
    stroke: ?tvg.Color = null,
    @"stroke-opacity": Number = .{ .bits = 1, .frac_digits = 0 },
    @"stroke-width": Number = .{ .bits = 1, .frac_digits = 0 },
    @"stroke-linecap": StrokeLineCap = StrokeLineCap.default(),
    @"stroke-linejoin": StrokeLineJoin = StrokeLineJoin.default(),

    pub fn getFillColor(self: InheritableAttributes) ?tvg.Color {
        const color = self.fill orelse return null;
        return applyOpacity(
            applyOpacity(color, self.@"fill-opacity"),
            self.opacity,
        );
    }
    pub fn getStrokeColor(self: InheritableAttributes) ?tvg.Color {
        const color = self.stroke orelse return null;
        return applyOpacity(
            applyOpacity(color, self.@"stroke-opacity"),
            self.opacity,
        );
    }

    const PathOp = struct {
        name: []const u8,
        fill_color: ?tvg.Color,
        stroke: ?struct {
            color: tvg.Color,
            width: Number,
        },
    };
    pub fn getTvgPathOp(self: InheritableAttributes) ?PathOp {
        if (self.getFillColor()) |fill_color| {
            if (self.getStrokeColor()) |stroke_color| return .{
                .name = "outline_fill_path",
                .fill_color = fill_color,
                .stroke = .{
                    .color = stroke_color,
                    .width = self.@"stroke-width",
                },
            } else return .{
                .name = "fill_path",
                .fill_color = fill_color,
                .stroke = null,
            };
        }
        if (self.getStrokeColor()) |stroke_color| return .{
            .name = "draw_line_path",
            .fill_color = null,
            .stroke = .{
                .color = stroke_color,
                .width = self.@"stroke-width",
            },
        };
        return null;
    }
};

fn writeTvgt(
    arena: std.mem.Allocator,
    writer: anytype,
    document: xml.Document,
) !void {
    try writer.writeAll("(tvg 1 ");

    var defaults: InheritableAttributes = .{};

    var maybe_viewbox: ?[]const u8 = null;
    var maybe_width: ?[]const u8 = null;
    var maybe_height: ?[]const u8 = null;
    for (elementAttrs(document.root)) |attr| {
        const attr_name = attr.name.slice();
        if (root_attr_map.get(attr.name.slice())) |root_attr| switch (root_attr) {
            .width => maybe_width = attr.value.slice(),
            .height => maybe_height = attr.value.slice(),
            .viewBox => maybe_viewbox = attr.value.slice(),
            .fill => defaults.fill = parseColorOrNone(attr.value.slice()),
            .xmlns => {},
            .xmlns_xlink => {},
            .version => if (!std.mem.eql(u8, attr.value.slice(), "1.1")) fatal(
                "unexpected svg version {s}", .{attr.value.slice()}
            ),
        } else fatal("todo handle svg attribute '{s}'", .{attr_name});
    }

    var analysis = Analysis{ .allocator = arena };
    for (document.root.children()) |child_node_index| {
        try analyze(&analysis, child_node_index.v().element, defaults);
    }

    const viewbox: struct {
        min: XY(Number),
        size: XY(Number),
    } = blk: {
        if (maybe_viewbox) |viewbox_str| {
            const min_x = (try parseNumber(viewbox_str, 0)) orelse fatal("viewbox missing min-x", .{});
            const min_y = (try parseNumber(viewbox_str, min_x.after)) orelse fatal("viewbox missing min-y", .{});
            const width = (try parseNumber(viewbox_str, min_y.after)) orelse fatal("viewbox missing width", .{});
            const height = (try parseNumber(viewbox_str, width.after)) orelse fatal("viewbox missing height", .{});
            const end = skipWhitespaceOrCommas(viewbox_str, height.after);
            if (end != viewbox_str.len) fatal("viewbox has extra junk '{s}'", .{viewbox_str[end..]});
            break :blk .{
                .min = .{ .x = min_x.value, .y = min_y.value },
                .size = .{ .x = width.value, .y = height.value },
            };
        }

        const width_str = maybe_width orelse @panic("svg has neither a viewBox nor a width");
        const height_str = maybe_height orelse @panic("svg has neither a viewBox nor a height");
        const width = Number.parse(width_str) catch |err| fatal(
            "parse width '{s}' failed with {s}",
            .{width_str, @errorName(err)}
        );
        const height = Number.parse(height_str) catch |err| fatal(
            "parse height '{s}' failed with {s}",
            .{height_str, @errorName(err)}
        );
        if (width.bits <= 0) fatal("width {} is not positive", .{width});
        if (height.bits <= 0) fatal("height {} is not positive", .{height});
        break :blk .{
            .min = .{
                .x = .{ .bits = 0, .frac_digits = 0 },
                .y = .{ .bits = 0, .frac_digits = 0 },
            },
            .size = .{ .x = width, .y = height },
        };
    };

    if (viewbox.min.x.bits != 0 or viewbox.min.y.bits != 0) fatal(
        "non-origin viewbox {},{} is not implemented, need to shift all points",
        .{viewbox.min.x, viewbox.min.y},
    );
    //analysis.addNumber(viewbox.min.x);
    //analysis.addNumber(viewbox.min.y);
    analysis.coordinates.add(viewbox.size.x);
    analysis.coordinates.add(viewbox.size.y);

    const scale: tvg.Scale = blk: {
        // NOTE: coordinates are separated from values because
        //       coordinates will need to be shifted based on the
        //       viewbox origin

        const min_max = analysis.getTvgMinMax();
        //const range = min_max.max.add(min_max.min.abs());
        std.log.info("SVG range is {} to {}", .{
            min_max.min, min_max.max
        });
        const svg_min_float = min_max.min.toFloat(f64);
        const svg_max_float = min_max.max.toFloat(f64);

        var scale: tvg.Scale = .@"1/32768";
        while (true) {
            const scale_min = @as(i16, @bitCast(@as(u16, 0x8000))) >> scale.getShiftBits();
            const scale_max_int_part = @as(u16, 0x7fff) >> scale.getShiftBits();
            const scale_max_decimal_part =
                @as(f64, @floatFromInt(@as(u32, 0x7fff) >> (15-scale.getShiftBits()))) /
                @as(f64, @floatFromInt(@as(u32, 0x8000) >> (15-scale.getShiftBits()))) ;
            const scale_max: f64 = @as(f64, @floatFromInt(scale_max_int_part)) + scale_max_decimal_part;
            const scale_min_step = 1 / @as(f64, @floatFromInt(@as(u32, 0x8000) >> (15-scale.getShiftBits())));

            const svg_min_ok = (svg_min_float >= @as(f64, @floatFromInt(scale_min)));
            const svg_max_ok = (svg_max_float <= scale_max);

            std.log.info(
                "scale {s: >7} min {s: <8} max {s: <8} (range {: >6} to {d: <17} | step size is {d})",
                .{
                    @tagName(scale),
                    @as([]const u8, if (svg_min_ok) "OK" else "OVERFLOW"),
                    @as([]const u8, if (svg_max_ok) "OK" else "OVERFLOW"),
                    scale_min,
                    scale_max,
                    scale_min_step,
                },
            );
            if (svg_min_ok and svg_max_ok)
                break :blk scale;
            if (scale == .@"1/1") fatal("svg too big to represent accurately?", .{});
            scale = @enumFromInt(@intFromEnum(scale)-1);
        }
    };
    std.log.warn("TODO: analyze the SVG to see what color encoding we should use", .{});
    std.log.warn("TODO: analyze the SVG to see what range we should use if the user didn't provide one (8/16/32 bit)", .{});
    const range: tvg.Range= .default;

    // the header
    try writer.print("({} {} {s} u8888 {s}) (\n", .{
        viewbox.size.x,
        viewbox.size.y,
        @tagName(scale),
        @tagName(range),
    });
    // the color table
    for (analysis.color_set.keys()) |color| {
        if (color.a != 1.0) try writer.print("  ({d} {d} {d} {d})\n", .{
            color.r, color.g, color.b, color.a
        }) else try writer.print("  ({d} {d} {d})\n", .{
            color.r, color.g, color.b
        });
    }
    try writer.writeAll(")(\n");
    // the draw commands
    for (document.root.children()) |child_node_index| {
        try writeTvgtCommands(
            writer,
            child_node_index.v().element,
            defaults,
            analysis,
        );
    }
    try writer.writeAll("))\n");
}

fn writeTvgtCommands(
    writer: anytype,
    element: xml.Element,
    defaults: InheritableAttributes,
    analysis: Analysis,
) !void {
    const element_name = element.tag_name.slice();
    const kind = element_map.get(element_name) orelse fatal(
        "unhandled element <{s}>", .{element_name}
    );

    if (element.children().len > 0) {
        if (!kind.allowsChildren()) fatal(
            "<{s}> does not allow child elements", .{element_name}
        );
    }

    var inheritable = defaults;
    var child_inheritable = defaults;
    applyAttributes(
        element,
        kind,
        &inheritable,
        &child_inheritable,
    );

    switch (kind) {
        .title => { },
        .path => {
            const path = parsePathAttrs(element);
            const d = path.d orelse return;
            const path_op = inheritable.getTvgPathOp() orelse return;
            try writer.print("  ({s}", .{path_op.name});
            if (path_op.fill_color) |color| {
                try writer.print(" {}", .{analysis.fmtColorStyle(color)});
            }
            if (path_op.stroke) |stroke| {
                if (inheritable.@"stroke-linecap" != StrokeLineCap.default()) {
                    std.log.warn(
                        "stroke-linecap is '{s}', can't render this, can only render '{s}'. " ++
                        "We should check if the path we are rendering is going to have an exposed linecap.",
                        .{@tagName(inheritable.@"stroke-linecap"), @tagName(StrokeLineCap.default())},
                    );
                }
                try writer.print(" {} {}", .{
                    analysis.fmtColorStyle(stroke.color),
                    stroke.width,
                });
            }
            try writer.writeAll(" (\n");
            try writePath(writer, d);
            try writer.print("  ))\n", .{});
        },
        .circle => {
            const circle = parseCircleAttrs(element);
            // the default radius is 0 which I think means it doesn't render anything
            const r = circle.r orelse return;
            const path_op = inheritable.getTvgPathOp() orelse return;
            try writer.print("  ({s}", .{path_op.name});
            if (path_op.fill_color) |color| {
                try writer.print(" {}", .{analysis.fmtColorStyle(color)});
            }
            if (path_op.stroke) |stroke| {
                if (inheritable.@"stroke-linecap" != StrokeLineCap.default()) {
                    std.log.warn(
                        "stroke-linecap is '{s}', can't render this, can only render '{s}'. " ++
                        "We should check if the path we are rendering is going to have an exposed linecap.",
                        .{@tagName(inheritable.@"stroke-linecap"), @tagName(StrokeLineCap.default())},
                    );
                }
                try writer.print(" {} {}", .{
                    analysis.fmtColorStyle(stroke.color),
                    stroke.width,
                });
            }
            try writer.writeAll(" (\n");
            const rad_abs = r.abs();
            const rad_neg = rad_abs.negate();
            const path_y = circle.cy.add(rad_neg);
            try writer.print("    ({} {}) (\n", .{circle.cx, path_y});
            try writer.print(
                "      (arc_circle - {} false false ({} {}))\n",
                .{rad_abs, circle.cx, circle.cy.add(rad_abs)},
            );
            try writer.print(
                "      (arc_circle - {} false false ({} {}))\n",
                .{rad_abs, circle.cx, path_y},
            );
            try writer.print("    )\n", .{});

            try writer.print("  ))\n", .{});
        },
        .group => {
            const children = element.children();
            for (children) |child| {
                try writeTvgtCommands(
                    writer,
                    child.v().element,
                    child_inheritable,
                    analysis,
                );
            }
        },
    }
}

fn applyOpacity(color: tvg.Color, opacity: Number) tvg.Color {
    if (opacity.equalsInteger(1))
        return color;
    return .{
        .r = color.r,
        .g = color.g,
        .b = color.b,
        .a = opacity.toFloat(f32) * color.a,
    };
}

fn writePath(
    writer: anytype,
    d: []const u8,
) !void {
    var it = PathDrawIterator{ .draw = d };
    var current_path = false;
    var position: XY(Number) = .{
        .x = .{.bits = 0, .frac_digits = 0 },
        .y = .{.bits = 0, .frac_digits = 0 },
    };
    var maybe_last_open_point: ?XY(Number) = null;
    while (try it.next()) |cmd| {
        const previous_position = position;
        position = cmd.calcPosition(
            previous_position,
            &maybe_last_open_point,
        );
        switch (cmd) {
            .m => {
                if (current_path) {
                    try writer.print("    )\n", .{});
                }
                try writer.print("    ({} {}) (\n", .{position.x, position.y});
                current_path = true;
            },
            .z => try writer.print("      (close - )\n", .{}),
            .h => try writer.print("      (horiz - {})\n", .{position.x}),
            .v => try writer.print("      (vert - {})\n", .{position.y}),
            .l => try writer.print("      (line - {} {})\n", .{position.x, position.y}),
            .a => |a| {
                try writer.print("      (arc_ellipse -", .{});
                try writer.print(" {}", .{a.arc.radius.x.abs()});
                try writer.print(" {}", .{a.arc.radius.y.abs()});
                try writer.print(" {}", .{a.arc.angle});
                try writer.print(" {}", .{a.arc.large_arc});
                try writer.print(" {}", .{switch (a.arc.sweep) { .left => true, .right => false }});
                try writer.print(" ({} {})", .{position.x, position.y});
                try writer.print(")\n", .{});
            },
            .s => |s| switch (s.kind) {
                .relative => try writer.print("      (quadratic_bezier - ({} {}) ({} {}))\n", .{
                    previous_position.x.add(s.bezier[0].x),
                    previous_position.y.add(s.bezier[0].y),
                    position.x,
                    position.y,
                }),
                .absolute => try writer.print("      (quadratic_bezier - ({} {}) ({} {}))\n", .{
                    s.bezier[0].x, s.bezier[0].y,
                    s.bezier[1].x, s.bezier[1].y,
                }),
            },
            .c => |c| switch (c.kind) {
                .relative => try writer.print("      (bezier - ({} {}) ({} {}) ({} {}))\n", .{
                    previous_position.x.add(c.bezier[0].x),
                    previous_position.y.add(c.bezier[0].y),
                    previous_position.x.add(c.bezier[1].x),
                    previous_position.y.add(c.bezier[1].y),
                    position.x,
                    position.y,
                }),
                .absolute => try writer.print("      (bezier - ({} {}) ({} {}) ({} {}))\n", .{
                    c.bezier[0].x, c.bezier[0].y,
                    c.bezier[1].x, c.bezier[1].y,
                    c.bezier[2].x, c.bezier[2].y,
                }),
            },
        }
    }
    if (current_path) {
        try writer.print("    )\n", .{});
    }
}

const FillRule = enum {
    nonzero, // default
    evenodd,

    pub fn default() FillRule { return .nonzero; }
    pub fn parse(str: []const u8) FillRule {
        return map.get(str) orelse std.debug.panic(
            "unknown fill-rule attribute '{s}'", .{str}
        );
    }
    const map = std.StaticStringMap(FillRule).initComptime(.{
        .{ "nonzero", .nonzero },
        .{ "evenodd", .evenodd },
    });
};
const StrokeLineCap = enum {
    butt, // default
    round,
    square,

    pub fn default() StrokeLineCap { return .butt; }
    pub fn parse(str: []const u8) StrokeLineCap {
        return map.get(str) orelse std.debug.panic(
            "unknown stroke-linecap attribute '{s}'", .{str}
        );
    }
    const map = std.StaticStringMap(StrokeLineCap).initComptime(.{
        .{ "butt", .butt },
        .{ "round", .round },
        .{ "square", .square },
    });
};
const StrokeLineJoin = enum {
    miter, // default
    arcs,
    bevel,
    @"miter-clip",
    round,

    pub fn default() StrokeLineJoin { return .miter; }
    pub fn parse(str: []const u8) StrokeLineJoin {
        return map.get(str) orelse std.debug.panic(
            "unknown stroke-linejoin attribute '{s}'", .{str}
        );
    }
    const map = std.StaticStringMap(StrokeLineJoin).initComptime(.{
        .{ "miter", .miter },
        .{ "arcs", .arcs },
        .{ "bevel", .bevel },
        .{ "miter-clip", .@"miter-clip" },
        .{ "round", .round },
    });
};

const ElementKind = enum {
    path,
    circle,
    title,
    group,
    pub fn allowsChildren(self: ElementKind) bool {
        return switch (self) {
            .path => false,
            .circle => false,
            .title => false,
            .group => true,
        };
    }
    pub fn performsFill(self: ElementKind) bool {
        return switch (self) {
            .path => true,
            .circle => true,
            .title => false,
            .group => false,
        };
    }
    pub fn performsStroke(self: ElementKind) bool {
        return switch (self) {
            .path => true,
            .circle => true,
            .title => false,
            .group => false,
        };
    }
};
const element_map = std.StaticStringMap(ElementKind).initComptime(.{
    .{ "path", .path },
    .{ "circle", .circle },
    .{ "title", .title },
    .{ "g", .group },
});
const PathAttr = enum {
    d,
    opacity,
    fill,
    @"fill-opacity",
    @"fill-rule",
    stroke,
    @"stroke-width",
    @"stroke-linecap",
    @"stroke-linejoin",
};
const path_attr_map = std.StaticStringMap(PathAttr).initComptime(.{
    .{ "d", .d },
    .{ "opacity", .opacity },
    .{ "fill", .fill },
    .{ "fill-opacity", .@"fill-opacity" },
    .{ "fill-rule", .@"fill-rule" },
    .{ "stroke", .stroke },
    .{ "stroke-width", .@"stroke-width" },
    .{ "stroke-linecap", .@"stroke-linecap" },
    .{ "stroke-linejoin", .@"stroke-linejoin" },
});
const CircleAttr = enum {
    fill,
    cx,
    cy,
    r,
};
const circle_attr_map = std.StaticStringMap(CircleAttr).initComptime(.{
    .{ "fill", .fill },
    .{ "cx", .cx },
    .{ "cy", .cy },
    .{ "r", .r },
});
const GroupAttr = enum {
    fill,
    @"fill-rule",
};
const group_attr_map = std.StaticStringMap(GroupAttr).initComptime(.{
    .{ "fill", .fill },
    .{ "fill-rule", .@"fill-rule" },
});

fn parseColorOrNone(str: []const u8) ?tvg.Color {
    if (std.mem.eql(u8, str, "none"))
        return null;
    return parseColor(str);
}
fn parseColor(str: []const u8) tvg.Color {
    if (str.len > 0 and str[0] == '#') return tvg.Color.fromString(
        str[1..],
    ) catch |err| switch (err) {
        error.Overflow,
        error.InvalidCharacter,
        error.InvalidFormat,
        => |e| std.debug.panic("invalid hex color '{s}' ({s})", .{str, @errorName(e)}),
    };
    if (named_colors.get(str)) |color|
        return color;
    std.debug.panic("todo handle color '{s}'", .{str});
}

const ColorHashMapUnmanaged = std.ArrayHashMapUnmanaged(
    tvg.Color,
    u32,
    ColorHashContext,
    false,
);

const Path = struct {
    d: ?[]const u8 = null,
};
fn parsePathAttrs(
    element: xml.Element,
) Path {
    var result: Path = .{};
    for (elementAttrs(element)) |attr| {
        const attr_name = attr.name.slice();
        const attr_value = attr.value.slice();
        switch (getAttr("path", path_attr_map, attr_name)) {
            .d => result.d = attr_value,
            .opacity => {},
            .fill => {},
            .@"fill-opacity" => {},
            .@"fill-rule" => {},
            .stroke => {},
            .@"stroke-width" => {},
            .@"stroke-linecap" => {},
            .@"stroke-linejoin" => {},
        }
    }
    return result;
}

const Circle = struct {
    cx: Number = Number{ .bits = 0, .frac_digits = 0 },
    cy: Number = Number{ .bits = 0, .frac_digits = 0 },
    r: ?Number = null,
};
fn parseCircleAttrs(
    element: xml.Element,
) Circle {
    var result: Circle = .{};
    for (elementAttrs(element)) |attr| {
        const attr_name = attr.name.slice();
        const attr_value = attr.value.slice();
        switch (getAttr("circle", circle_attr_map, attr_name)) {
            .fill => {},
            .cx => result.cx = Number.parse(attr_value) catch |err| fatal(
                "bad circle cx '{s}': {s}", .{attr_value, @errorName(err)},
            ),
            .cy => result.cy = Number.parse(attr_value) catch |err| fatal(
                "bad circle cy '{s}': {s}", .{attr_value, @errorName(err)},
            ),
            .r => result.r = Number.parse(attr_value) catch |err| fatal(
                "bad circle r '{s}': {s}", .{attr_value, @errorName(err)},
            ),
        }
    }
    return result;
}

const MinMax = struct {
    min: Number = .{ .bits = 0, .frac_digits = 0 },
    max: Number = .{ .bits = 0, .frac_digits = 0 },
    pub fn add(self: *MinMax, num: Number) void {
        if (num.order(self.min) == .lt) {
            self.min = num;
        }
        if (num.order(self.max) == .gt) {
            self.max = num;
        }
        }
};

const Analysis = struct {
    allocator: std.mem.Allocator,

    values: MinMax = .{},
    coordinates: MinMax = .{},
    color_set: ColorHashMapUnmanaged = .{},

    // TODO: this will need to take the viewbox origin
    //       so it can shift the coordinate values
    pub fn getTvgMinMax(self: Analysis) MinMax {
        var result = self.values;
        // TODO: shift self.coordinates by viewbox origin
        result.add(self.coordinates.min);
        result.add(self.coordinates.max);
        return result;
    }

    pub fn addColor(self: *Analysis, color: tvg.Color) void {
        const entry = self.color_set.getOrPut(self.allocator, color) catch |e| oom(e);
        if (!entry.found_existing) {
            const color_index = std.math.cast(u32, self.color_set.count() - 1) orelse fatal(
                "too many colors", .{}
            );
            entry.value_ptr.* = color_index;
        }
    }

    const FormatColorStyle = struct {
        index: u32,
        pub fn format(
            self: FormatColorStyle,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            try writer.print("(flat {})", .{self.index});
        }
    };
    pub fn fmtColorStyle(self: Analysis, color: tvg.Color) FormatColorStyle {
        const index = self.color_set.get(color) orelse std.debug.panic(
            "color '{}' was missed in analysis",
            .{ color },
        );
        return .{ .index = index };
    }
};

fn getAttr(
    element_name: []const u8,
    attr_map: anytype,
    attr_name: []const u8,
) @TypeOf(attr_map.get("").?) {
    return attr_map.get(attr_name) orelse fatal(
        "<{s}> element has unknown attribute '{s}'", .{element_name, attr_name}
    );
}


fn applyAttributes(
    element: xml.Element,
    kind: ElementKind,
    this_inheritable: *InheritableAttributes,
    child_inheritable: *InheritableAttributes,
) void {
    const element_name = element.tag_name.slice();
    for (elementAttrs(element)) |attr| {
        const attr_name = attr.name.slice();
        const attr_value = attr.value.slice();
        switch (kind) {
            .title => {},
            .path => switch (getAttr(element_name, path_attr_map, attr_name)) {
                .d => {},
                .opacity => this_inheritable.opacity = Number.parse(attr_value) catch |err| fatal(
                    "bad opacity '{s}': {s}", .{attr_value, @errorName(err)}
                ),
                .fill => this_inheritable.fill = parseColorOrNone(attr_value),
                .@"fill-opacity" => this_inheritable.@"fill-opacity" = Number.parse(attr_value) catch |err| fatal(
                    "bad fill-opacity '{s}': {s}", .{attr_value, @errorName(err)}
                ),
                .@"fill-rule" => this_inheritable.@"fill-rule" = FillRule.parse(attr_value),
                .stroke => this_inheritable.stroke = parseColorOrNone(attr_value),
                .@"stroke-width" => this_inheritable.@"stroke-width" = Number.parse(attr_value) catch |err| fatal(
                    "bad stroke-width '{s}': {s}", .{attr_value, @errorName(err)}
                ),
                .@"stroke-linecap" => this_inheritable.@"stroke-linecap" = StrokeLineCap.parse(attr_value),
                .@"stroke-linejoin" => this_inheritable.@"stroke-linejoin" = StrokeLineJoin.parse(attr_value),
            },
            .circle => switch (getAttr(element_name, circle_attr_map, attr_name)) {
                .fill => this_inheritable.fill = parseColorOrNone(attr_value),
                .cx => {},
                .cy => {},
                .r => {},
            },
            .group => switch (getAttr(element_name, group_attr_map, attr_name)) {
                .fill => child_inheritable.fill = parseColorOrNone(attr_value),
                .@"fill-rule" => child_inheritable.@"fill-rule" = FillRule.parse(attr_value),
            },
        }
    }
}


fn analyze(
    analysis: *Analysis,
    element: xml.Element,
    defaults: InheritableAttributes,
) !void {
    const element_name = element.tag_name.slice();
    const kind = element_map.get(element_name) orelse fatal(
        "unhandled element <{s}>", .{element_name}
    );

    var inheritable = defaults;
    var child_inheritable = defaults;
    applyAttributes(
        element,
        kind,
        &inheritable,
        &child_inheritable,
    );

    // go over attributes againt to analyze numbers
    switch (kind) {
        .title => {},
        .path => {
            const path = parsePathAttrs(element);
            if (path.d) |d| try analyzePathDraw(analysis, d);
        },
        .circle => {
            const circle = parseCircleAttrs(element);
            if (circle.r) |r| {
                analysis.values.add(r);
                analysis.coordinates.add(circle.cx);
                analysis.coordinates.add(circle.cy);
            }
        },
        .group => {},
    }

    if (kind.performsFill()) {
        if (inheritable.getFillColor()) |color| {
            analysis.addColor(color);
        }
    }
    if (kind.performsStroke()) {
        if (inheritable.getStrokeColor()) |color| {
            analysis.addColor(color);
        }
    }

    const children = element.children();
    if (children.len > 0) {
        if (!kind.allowsChildren()) fatal(
            "<{s}> does not allow child elements", .{element_name}
        );
        for (children) |child| {
            try analyze(analysis, child.v().element, child_inheritable);
        }
    }
}

fn XY(comptime T: type) type {
    return struct {
        x: T,
        y: T,
    };
}

const DrawPositionKind = enum { absolute, relative };

const PathDraw = union(enum) {
    m: struct {
        kind: DrawPositionKind,
        point: XY(Number),
    },
    z: void,
    h: struct {
        kind: DrawPositionKind,
        len: Number,
    },
    v: struct {
        kind: DrawPositionKind,
        len: Number,
    },
    l: struct {
        kind: DrawPositionKind,
        point: XY(Number),
    },
    a: struct {
        kind: DrawPositionKind,
        arc: Arc,
    },
    s: struct {
        kind: DrawPositionKind,
        bezier: [2]XY(Number),
    },
    c: struct {
        kind: DrawPositionKind,
        bezier: [3]XY(Number),
    },
    pub fn getExtraParamsCmd(self: PathDraw) SvgCmd {
        return switch (self) {
            .m => |m| switch (m.kind) { .absolute => .L, .relative => .l },
            .z => .Z,
            .h => |h| switch (h.kind) { .absolute => .H, .relative => .h },
            .v => |v| switch (v.kind) { .absolute => .V, .relative => .v },
            .l => |l| switch (l.kind) { .absolute => .L, .relative => .l },
            .a => |a| switch (a.kind) { .absolute => .A, .relative => .a },
            .s => |s| switch (s.kind) { .absolute => .S, .relative => .s },
            .c => |c| switch (c.kind) { .absolute => .C, .relative => .c },
        };
    }
    pub fn addValues(self: PathDraw, values: *MinMax) void {
        switch (self) {
            .m => {},
            .z => {},
            .h => {},
            .v => {},
            .l => {},
            .a => |a| {
                values.add(a.arc.radius.x.abs());
                values.add(a.arc.radius.y.abs());
                values.add(a.arc.angle);
            },
            .s => {},
            .c => {},
        }
    }
    pub fn calcPosition(
        self: PathDraw,
        current: XY(Number),
        maybe_last_open_point: *?XY(Number),
    ) XY(Number) {
        switch (self) {
            .m => |m| {
                const point: XY(Number) = switch (m.kind) {
                    .absolute => m.point,
                    .relative => .{
                        .x = current.x.add(m.point.x),
                        .y = current.y.add(m.point.y),
                    },
                };
                maybe_last_open_point.* = point;
                return point;
            },
            .z => {
                const last_open_point = maybe_last_open_point.* orelse fatal(
                    "svg closed the path without opening one", .{}
                );
                return last_open_point;
            },
            .h => |h| switch (h.kind) {
                .absolute => return .{
                    .x = h.len,
                    .y = current.y,
                },
                .relative => return .{
                    .x = current.x.add(h.len),
                    .y = current.y,
                },
            },
            .v => |v| switch (v.kind) {
                .absolute => return .{
                    .x = current.x,
                    .y = v.len,
                },
                .relative => return .{
                    .x = current.x,
                    .y = current.y.add(v.len),
                },
            },
            .l => |l| switch (l.kind) {
                .absolute => return l.point,
                .relative => return .{
                    .x = current.x.add(l.point.x),
                    .y = current.y.add(l.point.y),
                },
            },
            .a => |a| switch (a.kind) {
                .absolute => return a.arc.target,
                .relative => return .{
                    .x = current.x.add(a.arc.target.x),
                    .y = current.y.add(a.arc.target.y),
                },
            },
            .s => |s| switch (s.kind) {
                // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                // NOTE: we need to add the extra points to our number set
                //       probably add a second function addExtraPoints or something
                .absolute => return s.bezier[1],
                .relative => return .{
                    .x = current.x.add(s.bezier[1].x),
                    .y = current.y.add(s.bezier[1].y),
                },
            },
            .c => |c| switch (c.kind) {
                // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                // NOTE: we need to add the extra points to our number set
                //       probably add a second function addExtraPoints or something
                .absolute => return c.bezier[2],
                .relative => return .{
                    .x = current.x.add(c.bezier[2].x),
                    .y = current.y.add(c.bezier[2].y),
                },
            },
        }
    }
};
const PathDrawIterator = struct {
    draw: []const u8,
    index: usize = 0,
    last_command: ?SvgCmd = null,

    pub fn next(self: *PathDrawIterator) !?PathDraw {
        while (true) {
            const result = blk: {
                const first_result = try scanDrawCommand(self.draw, self.index);
                if (first_result.after != self.index)
                    break :blk first_result;
                if (self.index == self.draw.len)
                    return null;
                if (self.last_command) |last_command| {
                    const second_result = try parseCommand(self.draw, self.index, last_command);
                    if (second_result.after != self.index)
                        break :blk second_result;
                }
                std.debug.panic(
                    "invalid path draw \"{s}\" unknown character '{c}' at index {}",
                    .{self.draw, self.draw[self.index], self.index },
                );
            };
            self.index = result.after;
            if (result.command) |c| {
                self.last_command = c.getExtraParamsCmd();
                return c;
            }
        }
    }
};

const SvgCmdParams = enum {
    none,
    number,
    point,
    arc,
    quadratic_bezier,
    cubic_bezier,
};
const SvgCmd = enum {
    M, H, V, L, A, S, C, Z,
    m, h, v, l, a, s, c,

    pub fn initNumber(self: SvgCmd, n: Number) PathDraw {
        return switch (self) {
            .H => .{ .h = .{ .kind = .absolute, .len = n } },
            .V => .{ .v = .{ .kind = .absolute, .len = n } },
            .h => .{ .h = .{ .kind = .relative, .len = n } },
            .v => .{ .v = .{ .kind = .relative, .len = n } },
            .M, .L, .A, .S, .C, .Z,
            .m, .l, .a, .s, .c,
            => @panic("codebug"),
        };
    }
    pub fn initPoint(self: SvgCmd, p: XY(Number)) PathDraw {
        return switch (self) {
            .M => .{ .m = .{ .kind = .absolute, .point = p } },
            .L => .{ .l = .{ .kind = .absolute, .point = p } },
            .m => .{ .m = .{ .kind = .relative, .point = p } },
            .l => .{ .l = .{ .kind = .relative, .point = p } },
            .H, .V, .A, .S, .C, .Z,
            .h, .v, .a, .s, .c,
            => @panic("codebug"),
        };
    }

    pub fn fromChar(c: u8) ?SvgCmd {
        return switch (c) {
            'A' => .A,
            'C' => .C,
            'H' => .H,
            'L' => .L,
            'M' => .M,
            'S' => .S,
            'V' => .V,
            'Z' => .Z,
            'a' => .a,
            'c' => .c,
            'h' => .h,
            'l' => .l,
            'm' => .m,
            's' => .s,
            'v' => .v,
            'z' => .Z,
            else => null,
        };
    }
    pub fn getPositionKind(self: SvgCmd) DrawPositionKind {
        return switch (self) {
            .M, .H, .V, .L, .A, .S, .C, .Z, => .absolute,
            .m, .h, .v, .l, .a, .s, .c => .relative,
        };
    }
    pub fn getParams(self: SvgCmd) SvgCmdParams {
        return switch (self) {
            .M => .point,
            .H => .number,
            .V => .number,
            .L => .point,
            .A => .arc,
            .S => .quadratic_bezier,
            .C => .cubic_bezier,
            .Z => .none,
            .m => .point,
            .h => .number,
            .v => .number,
            .l => .point,
            .a => .arc,
            .s => .quadratic_bezier,
            .c => .cubic_bezier,
        };
    }
};

const ScanCommandResult = struct {
    after: usize,
    command: ?PathDraw,
};
fn scanDrawCommand(draw: []const u8, start: usize) !ScanCommandResult {
    var index = skipWhitespaceOrCommas(draw, start);
    if (index >= draw.len) return .{
        .after = index,
        .command = null,
    };

    if (SvgCmd.fromChar(draw[index])) |cmd| {
        index += 1;
        return try parseCommand(draw, index, cmd);
    }

    return .{ .after = index, .command = null };
}

fn parseCommand(draw: []const u8, start: usize, cmd: SvgCmd) !ScanCommandResult {
    switch (cmd.getParams()) {
        .none => return .{
            .after = start,
            .command = .z,
        },
        .number => if (try parseNumber(draw, start)) |result| return .{
            .after = result.after,
            .command = cmd.initNumber(result.value),
        },
        .point => if (try parsePoint(draw, start)) |result| return .{
            .after = result.after,
            .command = cmd.initPoint(result.point),
        },
        .arc => if (try parseArc(draw, start)) |result| return .{
            .after = result.after,
            .command = .{ .a = .{
                .kind = cmd.getPositionKind(),
                .arc = result.arc,
            }},
        },
        .quadratic_bezier => if (try parseQuadraticBezier(draw, start)) |result| return .{
            .after = result.after,
            .command = .{ .s = .{
                .kind = cmd.getPositionKind(),
                .bezier = result.bezier,
            }},
        },
        .cubic_bezier => if (try parseCubicBezier(draw, start)) |result| return .{
            .after = result.after,
            .command = .{ .c = .{
                .kind = cmd.getPositionKind(),
                .bezier = result.bezier,
            }},
        },
    }
    return .{ .after = start, .command = null };
}

fn analyzePathDraw(
    analysis: *Analysis,
    draw: []const u8,
) !void {
    var position: XY(Number) = .{
        .x = .{.bits = 0, .frac_digits = 0 },
        .y = .{.bits = 0, .frac_digits = 0 },
    };
    var maybe_last_open_point: ?XY(Number) = null;

    var it = PathDrawIterator{ .draw = draw };
    while (try it.next()) |cmd| {
        // avoid "attack of the kill features"
        const copy = position;
        position = cmd.calcPosition(copy, &maybe_last_open_point);
        analysis.coordinates.add(position.x);
        analysis.coordinates.add(position.y);
        cmd.addValues(&analysis.values);
    }
}

fn parseNumber(s: []const u8, start: usize) !?struct {
    after: usize,
    value: Number,
} {
    var index = start;

    index = skipWhitespaceOrCommas(s, index);
    const parsed = (try Number.parseLazy(s[index..])) orelse return null;
    index += parsed.char_count;

    return .{ .after = index, .value = parsed.num };
}

fn parsePoint(s: []const u8, start: usize) !?struct {
    after: usize,
    point: XY(Number),
} {
    const x = (try parseNumber(s, start)) orelse return null;
    const y = (try parseNumber(s, x.after)) orelse fatal(
        "expected a number pair but only got a single number: '{s}'", .{s[start..]}
    );
    return .{
        .after = y.after,
        .point = .{ .x = x.value, .y = y.value },
    };
}

const Sweep = enum { left, right };
const Arc = struct {
    radius: XY(Number),
    angle: Number,
    large_arc: bool,
    sweep: Sweep,
    target: XY(Number),
};
fn parseArc(s: []const u8, start: usize) !?struct {
    after: usize,
    arc: Arc,
} {
    const radius_x = (try parseNumber(s, start)) orelse return null;
    const radius_y = (try parseNumber(s, radius_x.after)) orelse fatal(
        "arc missing y radius: '{s}'", .{s[start..]}
    );
    const angle = (try parseNumber(s, radius_y.after)) orelse fatal(
        "arc missing angle: '{s}'", .{s[start..]}
    );
    const large_arc = try parseFlag(s, angle.after);
    const sweep = try parseFlag(s, large_arc.after);
    const target_x = (try parseNumber(s, sweep.after)) orelse fatal(
        "arc missing target x: '{s}'", .{s[start..]}
    );
    const target_y = (try parseNumber(s, target_x.after)) orelse fatal(
        "arc missing target y: '{s}'", .{s[start..]}
    );
    return .{
        .after = target_y.after,
        .arc = .{
            .radius = .{ .x = radius_x.value, .y = radius_y.value },
            .angle = angle.value,
            .large_arc = large_arc.enabled,
            .sweep = if (sweep.enabled) .right else .left,
            .target = .{ .x = target_x.value, .y = target_y.value },
        },
    };
}

fn parseQuadraticBezier(s: []const u8, start: usize) !?struct {
    after: usize,
    bezier: [2]XY(Number),
} {
    const first_x = (try parseNumber(s, start)) orelse return null;
    const first_y = (try parseNumber(s, first_x.after)) orelse fatal(
        "bezier missing second coordinate: '{s}'", .{s[start..]}
    );
    const second = (try parsePoint(s, first_y.after)) orelse fatal(
        "bezier missing second point: '{s}'", .{s[start..]}
    );
    return .{
        .after = second.after,
        .bezier = [2]XY(Number){
            .{ .x = first_x.value, .y = first_y.value },
            second.point,
        },
    };
}

fn parseCubicBezier(s: []const u8, start: usize) !?struct {
    after: usize,
    bezier: [3]XY(Number),
} {
    const first_x = (try parseNumber(s, start)) orelse return null;
    const first_y = (try parseNumber(s, first_x.after)) orelse fatal(
        "bezier missing second coordinate: '{s}'", .{s[start..]}
    );
    const second = (try parsePoint(s, first_y.after)) orelse fatal(
        "bezier missing second point: '{s}'", .{s[start..]}
    );
    const third = (try parsePoint(s, second.after)) orelse fatal(
        "bezier missing third point: '{s}'", .{s[start..]}
    );
    return .{
        .after = third.after,
        .bezier = [3]XY(Number){
            .{ .x = first_x.value, .y = first_y.value },
            second.point,
            third.point,
        },
    };
}

fn skipWhitespaceOrCommas(s: []const u8, start: usize) usize {
    var index = start;
    while (
        index < s.len and (
            s[index] == '\n' or
                s[index] == ' ' or
                s[index] == ','
        )
    ) : (index += 1) { }
    return index;
}

fn parseFlag(s: []const u8, start: usize) !struct {
    after: usize,
    enabled: bool,
}{
    const result = (try parseNumber(s, start)) orelse fatal(
        "expected 0/1 flag but got '{s}'", .{s[start..]}
    );
    if (result.value.frac_digits != 0 or (result.value.bits != 0 and result.value.bits != 1)) fatal(
        "expected 0/1 flag but got '{s}'", .{s[start..result.after]}
    );
    return .{ .after = result.after, .enabled = (result.value.bits == 1) };
}

const ColorHashContext = struct {
    pub fn eql(ctx: ColorHashContext, a: tvg.Color, b: tvg.Color, index: usize) bool {
        _ = ctx;
        _ = index;
        return
            a.r == b.r and
            a.g == b.g and
            a.b == b.b and
            a.a == b.a;
    }
    pub fn hash(ctx: ColorHashContext, key: tvg.Color) u32 {
        _ = ctx;
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(@as([*]const u8, @ptrCast(&key.r))[0..4]);
        hasher.update(@as([*]const u8, @ptrCast(&key.g))[0..4]);
        hasher.update(@as([*]const u8, @ptrCast(&key.b))[0..4]);
        hasher.update(@as([*]const u8, @ptrCast(&key.a))[0..4]);
        return @truncate(hasher.final());
    }
};

pub const Attribute = extern struct {
    name: xml.StringIndex align(1),
    value: xml.StringIndex align(1),
};

pub fn elementAttrs(elem: xml.Element) []const Attribute {
    const eidx = elem.attributes;
    if (eidx == .empty) return &.{};
    const handle = xml.doc.?.data[@intFromEnum(eidx)..];
    const len = handle[0] / 2;
    const ptr: [*]const Attribute = @ptrCast(handle[1..].ptr);
    return ptr[0 .. len];
}

fn fromRgb(r: u8, g: u8, b: u8) tvg.Color {
    return .{
        .a = 1.0,
        .r = @as(f32, @floatFromInt(r)) / 255.0,
        .g = @as(f32, @floatFromInt(g)) / 255.0,
        .b = @as(f32, @floatFromInt(b)) / 255.0,
    };
}

const black = fromRgb(0, 0, 0);
const named_colors = std.StaticStringMap(tvg.Color).initComptime(.{
    .{ "aliceblue", fromRgb(240, 248, 255) },
    .{ "antiquewhite", fromRgb(250, 235, 215) },
    .{ "aqua", fromRgb(0, 255, 255) },
    .{ "aquamarine", fromRgb(127, 255, 212) },
    .{ "azure", fromRgb(240, 255, 255) },
    .{ "beige", fromRgb(245, 245, 220) },
    .{ "bisque", fromRgb(255, 228, 196) },
    .{ "black", black },
    .{ "blanchedalmond", fromRgb(255, 235, 205) },
    .{ "blue", fromRgb(0, 0, 255) },
    .{ "blueviolet", fromRgb(138, 43, 226) },
    .{ "brown", fromRgb(165, 42, 42) },
    .{ "burlywood", fromRgb(222, 184, 135) },
    .{ "cadetblue", fromRgb(95, 158, 160) },
    .{ "chartreuse", fromRgb(127, 255, 0) },
    .{ "chocolate", fromRgb(210, 105, 30) },
    .{ "coral", fromRgb(255, 127, 80) },
    .{ "cornflowerblue", fromRgb(100, 149, 237) },
    .{ "cornsilk", fromRgb(255, 248, 220) },
    .{ "crimson", fromRgb(220, 20, 60) },
    .{ "cyan", fromRgb(0, 255, 255) },
    .{ "darkblue", fromRgb(0, 0, 139) },
    .{ "darkcyan", fromRgb(0, 139, 139) },
    .{ "darkgoldenrod", fromRgb(184, 134, 11) },
    .{ "darkgray", fromRgb(169, 169, 169) },
    .{ "darkgreen", fromRgb(0, 100, 0) },
    .{ "darkgrey", fromRgb(169, 169, 169) },
    .{ "darkkhaki", fromRgb(189, 183, 107) },
    .{ "darkmagenta", fromRgb(139, 0, 139) },
    .{ "darkolivegreen", fromRgb(85, 107, 47) },
    .{ "darkorange", fromRgb(255, 140, 0) },
    .{ "darkorchid", fromRgb(153, 50, 204) },
    .{ "darkred", fromRgb(139, 0, 0) },
    .{ "darksalmon", fromRgb(233, 150, 122) },
    .{ "darkseagreen", fromRgb(143, 188, 143) },
    .{ "darkslateblue", fromRgb(72, 61, 139) },
    .{ "darkslategray", fromRgb(47, 79, 79) },
    .{ "darkslategrey", fromRgb(47, 79, 79) },
    .{ "darkturquoise", fromRgb(0, 206, 209) },
    .{ "darkviolet", fromRgb(148, 0, 211) },
    .{ "deeppink", fromRgb(255, 20, 147) },
    .{ "deepskyblue", fromRgb(0, 191, 255) },
    .{ "dimgray", fromRgb(105, 105, 105) },
    .{ "dimgrey", fromRgb(105, 105, 105) },
    .{ "dodgerblue", fromRgb(30, 144, 255) },
    .{ "firebrick", fromRgb(178, 34, 34) },
    .{ "floralwhite", fromRgb(255, 250, 240) },
    .{ "forestgreen", fromRgb(34, 139, 34) },
    .{ "fuchsia", fromRgb(255, 0, 255) },
    .{ "gainsboro", fromRgb(220, 220, 220) },
    .{ "ghostwhite", fromRgb(248, 248, 255) },
    .{ "gold", fromRgb(255, 215, 0) },
    .{ "goldenrod", fromRgb(218, 165, 32) },
    .{ "gray", fromRgb(128, 128, 128) },
    .{ "green", fromRgb(0, 128, 0) },
    .{ "greenyellow", fromRgb(173, 255, 47) },
    .{ "grey", fromRgb(128, 128, 128) },
    .{ "honeydew", fromRgb(240, 255, 240) },
    .{ "hotpink", fromRgb(255, 105, 180) },
    .{ "indianred", fromRgb(205, 92, 92) },
    .{ "indigo", fromRgb(75, 0, 130) },
    .{ "ivory", fromRgb(255, 255, 240) },
    .{ "khaki", fromRgb(240, 230, 140) },
    .{ "lavender", fromRgb(230, 230, 250) },
    .{ "lavenderblush", fromRgb(255, 240, 245) },
    .{ "lawngreen", fromRgb(124, 252, 0) },
    .{ "lemonchiffon", fromRgb(255, 250, 205) },
    .{ "lightblue", fromRgb(173, 216, 230) },
    .{ "lightcoral", fromRgb(240, 128, 128) },
    .{ "lightcyan", fromRgb(224, 255, 255) },
    .{ "lightgoldenrodyellow", fromRgb(250, 250, 210) },
    .{ "lightgray", fromRgb(211, 211, 211) },
    .{ "lightgreen", fromRgb(144, 238, 144) },
    .{ "lightgrey", fromRgb(211, 211, 211) },
    .{ "lightpink", fromRgb(255, 182, 193) },
    .{ "lightsalmon", fromRgb(255, 160, 122) },
    .{ "lightseagreen", fromRgb(32, 178, 170) },
    .{ "lightskyblue", fromRgb(135, 206, 250) },
    .{ "lightslategray", fromRgb(119, 136, 153) },
    .{ "lightslategrey", fromRgb(119, 136, 153) },
    .{ "lightsteelblue", fromRgb(176, 196, 222) },
    .{ "lightyellow", fromRgb(255, 255, 224) },
    .{ "lime", fromRgb(0, 255, 0) },
    .{ "limegreen", fromRgb(50, 205, 50) },
    .{ "linen", fromRgb(250, 240, 230) },
    .{ "magenta", fromRgb(255, 0, 255) },
    .{ "maroon", fromRgb(128, 0, 0) },
    .{ "mediumaquamarine", fromRgb(102, 205, 170) },
    .{ "mediumblue", fromRgb(0, 0, 205) },
    .{ "mediumorchid", fromRgb(186, 85, 211) },
    .{ "mediumpurple", fromRgb(147, 112, 219) },
    .{ "mediumseagreen", fromRgb(60, 179, 113) },
    .{ "mediumslateblue", fromRgb(123, 104, 238) },
    .{ "mediumspringgreen", fromRgb(0, 250, 154) },
    .{ "mediumturquoise", fromRgb(72, 209, 204) },
    .{ "mediumvioletred", fromRgb(199, 21, 133) },
    .{ "midnightblue", fromRgb(25, 25, 112) },
    .{ "mintcream", fromRgb(245, 255, 250) },
    .{ "mistyrose", fromRgb(255, 228, 225) },
    .{ "moccasin", fromRgb(255, 228, 181) },
    .{ "navajowhite", fromRgb(255, 222, 173) },
    .{ "navy", fromRgb(0, 0, 128) },
    .{ "oldlace", fromRgb(253, 245, 230) },
    .{ "olive", fromRgb(128, 128, 0) },
    .{ "olivedrab", fromRgb(107, 142, 35) },
    .{ "orange", fromRgb(255, 165, 0) },
    .{ "orangered", fromRgb(255, 69, 0) },
    .{ "orchid", fromRgb(218, 112, 214) },
    .{ "palegoldenrod", fromRgb(238, 232, 170) },
    .{ "palegreen", fromRgb(152, 251, 152) },
    .{ "paleturquoise", fromRgb(175, 238, 238) },
    .{ "palevioletred", fromRgb(219, 112, 147) },
    .{ "papayawhip", fromRgb(255, 239, 213) },
    .{ "peachpuff", fromRgb(255, 218, 185) },
    .{ "peru", fromRgb(205, 133, 63) },
    .{ "pink", fromRgb(255, 192, 203) },
    .{ "plum", fromRgb(221, 160, 221) },
    .{ "powderblue", fromRgb(176, 224, 230) },
    .{ "purple", fromRgb(128, 0, 128) },
    .{ "rebeccapurple", fromRgb(102, 51, 153) },
    .{ "red", fromRgb(255, 0, 0) },
    .{ "rosybrown", fromRgb(188, 143, 143) },
    .{ "royalblue", fromRgb(65, 105, 225) },
    .{ "saddlebrown", fromRgb(139, 69, 19) },
    .{ "salmon", fromRgb(250, 128, 114) },
    .{ "sandybrown", fromRgb(244, 164, 96) },
    .{ "seagreen", fromRgb(46, 139, 87) },
    .{ "seashell", fromRgb(255, 245, 238) },
    .{ "sienna", fromRgb(160, 82, 45) },
    .{ "silver", fromRgb(192, 192, 192) },
    .{ "skyblue", fromRgb(135, 206, 235) },
    .{ "slateblue", fromRgb(106, 90, 205) },
    .{ "slategray", fromRgb(112, 128, 144) },
    .{ "slategrey", fromRgb(112, 128, 144) },
    .{ "snow", fromRgb(255, 250, 250) },
    .{ "springgreen", fromRgb(0, 255, 127) },
    .{ "steelblue", fromRgb(70, 130, 180) },
    .{ "tan", fromRgb(210, 180, 140) },
    .{ "teal", fromRgb(0, 128, 128) },
    .{ "thistle", fromRgb(216, 191, 216) },
    .{ "tomato", fromRgb(255, 99, 71) },
    .{ "turquoise", fromRgb(64, 224, 208) },
    .{ "violet", fromRgb(238, 130, 238) },
    .{ "wheat", fromRgb(245, 222, 179) },
    .{ "white", fromRgb(255, 255, 255) },
    .{ "whitesmoke", fromRgb(245, 245, 245) },
    .{ "yellow", fromRgb(255, 255, 0) },
    .{ "yellowgreen", fromRgb(154, 205, 5) },
});
