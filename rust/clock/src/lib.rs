use std::fmt;

#[derive(Debug)]
pub struct Clock {
    hours: i32,
    minutes: i32,
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:02}:{:02}", self.hours, self.minutes)
    }
}

impl PartialEq for Clock {
    fn eq(&self, other: &Self) -> bool {
        self.hours == other.hours && self.minutes == other.minutes
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let hours = if minutes % 60 < 0 {
            hours - 1
        } else {
            hours
        };

        Clock {
            hours: match (hours + minutes / 60) % 24 {
                h if h < 0 => h + 24,
                h => h,
            },
            minutes: match minutes % 60 {
                m if m < 0 => m + 60,
                m => m,
            },
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Clock::new(self.hours, self.minutes + minutes)
    }
}
