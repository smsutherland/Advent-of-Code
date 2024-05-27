const std = @import("std");
const days = .{
    .day1 = @import("./01.zig").run,
};

fn get_day_num() !u32 {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try stdout.print("Pick a day: ", .{});

    var buf: [10]u8 = undefined;
    if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |input| {
        const day = try std.fmt.parseInt(u32, input, 10);
        if (day > 25) {
            return error.BadInput;
        } else {
            return day;
        }
    } else {
        return error.BadInput;
    }
}

pub fn main() !void {
    const day = try get_day_num();
    const output = switch (day) {
        1 => try days.day1(),
        else => {
            return;
        },
    };
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Part 1: {}\nPart 2: {}\n", .{ output[0], output[1] });
}
