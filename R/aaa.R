# Below this many elements, calling into Rust costs more than it saves.
RUST_MIN <- 1e4

# Before R 4.6.0, match() compared Dates by their printed day, so 1.5
# matched 1; since, by their numbers (`mtfrm.Date()`). Checked when called,
# not when installed, as the same library can serve another R.
dates_by_day <- function() getRversion() < "4.6.0"
