package space

func init() {
	orbitalPeriodOf = createOrbitalTable()
}

func createOrbitalTable() map[Planet]float64 {
	planetYearInSeconds := func(orbitalPeriodDurationInEarthYears float64) float64 {
		const (
			earthYearInSeconds = 60 * 60 * 24 * 365.25
		)
		return earthYearInSeconds * orbitalPeriodDurationInEarthYears
	}

	return map[Planet]float64{
		"Earth":   planetYearInSeconds(1.0),
		"Mercury": planetYearInSeconds(0.2408467),
		"Venus":   planetYearInSeconds(0.61519726),
		"Mars":    planetYearInSeconds(1.8808158),
		"Jupiter": planetYearInSeconds(11.862615),
		"Saturn":  planetYearInSeconds(29.447498),
		"Uranus":  planetYearInSeconds(84.016846),
		"Neptune": planetYearInSeconds(164.79132),
	}
}

var orbitalPeriodOf map[Planet]float64

// Planet represents a planet
type Planet string

// Age is the age of a planet.
func Age(seconds float64, planet Planet) float64 {
	return seconds / orbitalPeriodOf[planet]
}
