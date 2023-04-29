/*
 * Shifting left goes up a file, i.e. A -> B file. But when in H file that wraps
 * it back into the A file, so when shifting left we need to mask out the A file.
 * And vice versa for shifting right.
 */

pub const NOT_A_FILE: u64 = 0xFEFEFEFEFEFEFEFE;
pub const NOT_H_FILE: u64 = 0x7F7F7F7F7F7F7F7F;
pub const NOT_A_B_FILE: u64 = 0xFCFCFCFCFCFCFCFC;
pub const NOT_G_H_FILE: u64 = 0x3F3F3F3F3F3F3F3F;

// direction_macros.rs
#[macro_export]
macro_rules! north {
    ($value:expr, $shift_amount:expr) => {
        $value << (8 * $shift_amount)
    };
}

#[macro_export]
macro_rules! south {
    ($value:expr, $shift_amount:expr) => {
        $value >> (8 * $shift_amount)
    };
}

#[macro_export]
macro_rules! east {
    ($value:expr, $shift_amount:expr) => {
        $value << $shift_amount
    };
}

#[macro_export]
macro_rules! west {
    ($value:expr, $shift_amount:expr) => {
        $value >> $shift_amount
    };
}

#[macro_export]
macro_rules! north_east {
    ($value:expr) => {
        $value << 9
    };
}

#[macro_export]
macro_rules! north_west {
    ($value:expr) => {
        $value << 7
    };
}

#[macro_export]
macro_rules! south_east {
    ($value:expr) => {
        $value >> 7
    };
}

#[macro_export]
macro_rules! south_west {
    ($value:expr) => {
        $value >> 9
    };
}

#[macro_export]
macro_rules! north_north_east {
    ($value:expr) => {
        $value << 17
    };
}

#[macro_export]
macro_rules! north_north_west {
    ($value:expr) => {
        $value << 15
    };
}

#[macro_export]
macro_rules! north_east_east {
    ($value:expr) => {
        $value << 10
    };
}

#[macro_export]
macro_rules! north_west_west {
    ($value:expr) => {
        $value << 6
    };
}

#[macro_export]
macro_rules! south_south_east {
    ($value:expr) => {
        $value >> 15
    };
}

#[macro_export]
macro_rules! south_south_west {
    ($value:expr) => {
        $value >> 17
    };
}

#[macro_export]
macro_rules! south_east_east {
    ($value:expr) => {
        $value >> 6
    };
}

#[macro_export]
macro_rules! south_west_west {
    ($value:expr) => {
        $value >> 10
    };
}

pub(crate) use east;
pub(crate) use north;
pub(crate) use north_east;
pub(crate) use north_east_east;
pub(crate) use north_north_east;
pub(crate) use north_north_west;
pub(crate) use north_west;
pub(crate) use north_west_west;
pub(crate) use south;
pub(crate) use south_east;
pub(crate) use south_east_east;
pub(crate) use south_south_east;
pub(crate) use south_south_west;
pub(crate) use south_west;
pub(crate) use south_west_west;
pub(crate) use west;
