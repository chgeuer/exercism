package gigasecond

import "time"

const (
	oneGigaSecond = time.Duration(1000*1000*1000) * time.Second
)

// AddGigasecond adds a billion seconds to the argument.
func AddGigasecond(t time.Time) time.Time {
	return t.Add(oneGigaSecond)
}
