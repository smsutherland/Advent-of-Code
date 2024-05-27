const std = @import("std");
const fs = std.fs;
const common = @import("common.zig");

pub fn run() ![2]u32 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const input = try common.read_input(1, allocator);
    defer allocator.free(input);

    var stream = std.io.fixedBufferStream(input);
    var input_reader = stream.reader();
    var elves = std.ArrayList(u32).init(allocator);
    defer elves.deinit();

    blk: while (true) { // each elf
        var elf: u32 = 0;
        while (true) // each item
        {
            var buf: [64]u8 = undefined;
            if (try input_reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
                if (line.len == 0) {
                    break;
                }
                elf += try std.fmt.parseUnsigned(u32, line, 10);
            } else {
                break :blk;
            }
        }
        try elves.append(elf);
    }

    std.mem.sort(u32, elves.items, .{}, gt);

    return [_]u32{ elves.items[0], elves.items[0] + elves.items[1] + elves.items[2] };
}

fn gt(_: @TypeOf(.{}), a: u32, b: u32) bool {
    return a > b;
}
