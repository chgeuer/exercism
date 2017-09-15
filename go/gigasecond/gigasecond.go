package gigasecond

import (
	"time"
)

const testVersion = 4

const oneGigaSecond = time.Duration(1000*1000*1000) * time.Second

func AddGigasecond(dateOfBirth time.Time) time.Time {
	return dateOfBirth.Add(oneGigaSecond)
}