use std::ops::Add;
use chrono::{DateTime, Utc, Duration};

// Returns a Utc DateTime one billion seconds after start.
pub fn after(start: DateTime<Utc>) -> DateTime<Utc> {
    let one_billion_seconds = Duration::seconds(1_000_000_000);
 
    start.add(one_billion_seconds)
}