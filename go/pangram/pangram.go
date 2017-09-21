package pangram

import (
	"strings"
)

const testVersion = 2

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
