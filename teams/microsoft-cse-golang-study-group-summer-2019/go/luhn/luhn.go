package luhn

import (
	"fmt"
	"strings"
)

// Valid returns true if the input is valid.
func Valid(input string) bool {
	// Spaces are allowed in the input, but they should be stripped before checking.
	trimmed := strings.Replace(input, " ", "", -1)

	// Strings of length 1 or less are not valid.
	length := len(trimmed)
	if length <= 1 {
		return false
	}

	idx := length % 2
	shouldBeDoubled := func(index int) bool {
		return index%2 == idx
	}

	charToNumber := func(digit rune) (int, error) {
		if digit < '0' || digit > '9' {
			return 0, fmt.Errorf("%c is not a decimal digit", digit)
		}
		return int(digit - '0'), nil
	}

	doubleDigit := func(digit int) int {
		double := digit * 2
		if double > 9 {
			return double - 9
		}
		return double
	}

	sum := 0
	for index, c := range trimmed {
		n, err := charToNumber(c)
		if err != nil {
			return false
		}

		if shouldBeDoubled(index) {
			sum += doubleDigit(n)
		} else {
			sum += n
		}
	}

	return sum%10 == 0
}
