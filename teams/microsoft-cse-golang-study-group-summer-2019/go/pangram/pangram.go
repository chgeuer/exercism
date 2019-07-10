package pangram

import (
	"strings"
)

// IsPangram returns true if the input string is a Pangram.
func IsPangram(input string) bool {
	// return isPangramMap(input)
	return isPangramArray(input)
	// return isPangramExtremelyFunctional(input)
}

func isPangramMap(input string) bool {
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

func isPangramArray(input string) bool {
	var m [26]bool

	for _, r := range []rune(strings.ToUpper(input)) {
		i := int(r - 'A')
		if i > -1 && i < 26 {
			m[i] = true
		}
	}

	for _, requiredRune := range m {
		if !requiredRune {
			return false
		}
	}
	return true
}

func isPangramExtremelyFunctional(input string) bool {
	type twentysixBits [26]bool

	analyzeString := func(s string) twentysixBits {
		getRuneID := func(r rune) int { return int(r - 'A') }
		runeIDInAlphabet := func(i int) bool { return i > -1 && i < 26 }

		var m twentysixBits
		for _, r := range []rune(strings.ToUpper(input)) {
			i := getRuneID(r)
			if runeIDInAlphabet(i) {
				m[i] = true
			}
		}
		return m
	}
	allBitsTrue := func(inputBits twentysixBits) bool {
		for _, bit := range inputBits {
			if !bit {
				return false
			}
		}
		return true
	}

	return allBitsTrue(analyzeString(input))
}
