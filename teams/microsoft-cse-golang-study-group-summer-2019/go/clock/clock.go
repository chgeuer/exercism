package clock

import (
	"fmt"
)

// Clock represents a wall clock time
type Clock struct {
	h, m int
}

// New creates a new Clock instance.
func New(hour, minute int) Clock {
	return fromMinutes(60*hour + minute)
}

// Add adds the given minutes to the time.
func (clock Clock) Add(minutes int) Clock {
	return fromMinutes(60*clock.h + clock.m + minutes)
}

// Subtract subtracts the given minutes to the time.
func (clock Clock) Subtract(minutes int) Clock {
	return fromMinutes(60*clock.h + clock.m - minutes)
}

func (clock Clock) String() string {
	return fmt.Sprintf("%02d:%02d", clock.h, clock.m)
}

func fromMinutes(minutesTotal int) Clock {
	m := minutesTotal % 60
	h := (minutesTotal / 60) % 24

	if m < 0 {
		m = m + 60
		h = h - 1
	}
	if h < 0 {
		h = h + 24
	}

	return Clock{
		h: h,
		m: m,
	}
}
