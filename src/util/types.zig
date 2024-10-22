/// Coordinate represents a line-column position
/// in a text file.
pub const Coordinate = struct {
    /// 0-indexed line number.
    line: u32,
    /// 0-indexed column.
    column: u32,
};

/// Start and end position in a text file.
pub const Range = struct {
    start: Coordinate,
    end: Coordinate,
};

/// Start and end byte offsets in a text file.
pub const Span = struct {
    start: u32,
    end: u32,
};

pub const Diagnostic = struct {
    coord: Coordinate,
    message: []const u8,
};
