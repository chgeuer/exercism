// Package acronym should have a package comment that summarizes what it's about.
package acronym

import (
	"regexp"
	"strings"
)

// Abbreviate abbreviates words into a TLA.
func Abbreviate(s string) string {
	// \\p{Ll} a lowercase letter that has an uppercase variant.
	// \\p{Lu} an uppercase letter that has a lowercase variant.
	// \\p{P}  any kind of punctuation character

	l1 := regexp.MustCompile("([\\p{Ll}])([\\p{Lu}])").ReplaceAllString(s, "$1 $2")

	l2 := strings.Replace(l1, "'", "", -1)

	l3 := regexp.MustCompile("[\\p{P}\\s]").Split(l2, -1)

	l4 := make([]string, 0)
	for i := 0; i < len(l3); i++ {
		runes := []rune(l3[i])
		if len(runes) > 0 {
			l4 = append(l4, string(runes[0]))
		}
	}

	l5 := strings.Join(l4, "")

	l6 := strings.ToUpper(l5)

	return l6
}
