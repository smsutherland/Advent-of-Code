const std = @import("std");
const common = @import("common.zig");

const RPS = enum(u3) {
    rock = 1,
    paper = 2,
    scissors = 3,
};

fn score_v1(us: RPS, opponent: RPS) u32 {
    var score: u32 = @intFromEnum(us);

    const points = [_]u32{ 3, 6, 0 };
    score += points[(3 + @intFromEnum(us) - @intFromEnum(opponent)) % 3];

    return score;
}

fn score_v2(us: RPS, opponent: RPS) u32 {
    const points = [_]u32{ 0, 3, 6 };
    var score: u32 = points[@intFromEnum(us) - 1];

    score += (@intFromEnum(us) + @intFromEnum(opponent)) % 3 + 1;

    return score;
}

pub fn run() ![2]u32 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const input = try common.read_input(2, allocator);
    defer allocator.free(input);
    var stream = std.io.fixedBufferStream(input);
    var input_reader = stream.reader();

    var games = std.ArrayList([2]RPS).init(allocator);
    defer games.deinit();

    var buf: [4]u8 = undefined;
    while (try input_reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var this_game: [2]RPS = undefined;
        this_game[1] = @enumFromInt(line[0] - ('A' - 1));
        this_game[0] = @enumFromInt(line[2] - ('X' - 1));
        try games.append(this_game);
    }

    var part1: u32 = 0;
    var part2: u32 = 0;
    for (games.items) |game| {
        part1 += score_v1(game[0], game[1]);
        part2 += score_v2(game[0], game[1]);
    }

    return [_]u32{ part1, part2 };
}
