package pangram

import (
	"strings"
)

// IsPangram returns true if the input string is a Pangram.
func IsPangram(input string) bool {
	m := make(map[rune]bool)
	for _, r := range []rune(strings.ToUpper(input)) {
		m[r] = true
	}

	for _, requiredRune := range []rune("ABCDEFGHIJKLMNOPQRSTUVWXYZ") {
		_, requiredRunFound := m[requiredRune]
		if !requiredRunFound {
			return false
		}
	}
	return true
}
