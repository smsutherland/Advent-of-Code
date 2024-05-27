const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn read_file(file_name: []u8, allocator: Allocator) ![]u8 {
    const input_file = try std.fs.cwd().openFile(file_name, .{});
    defer input_file.close();

    return input_file.readToEndAlloc(allocator, 0xFFFF);
}

pub fn read_input(day_num: u32, allocator: Allocator) ![]u8 {
    var day_str: [2]u8 = "00".*;
    day_str[0] = @intCast((day_num / 10) % 10);
    day_str[1] = @intCast(day_num % 10);
    day_str[0] += '0';
    day_str[1] += '0';
    const path = "./input/day" ++ day_str ++ ".txt";
    return read_file(path, allocator);
}
