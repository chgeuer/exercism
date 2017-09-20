package acronym

import (
	"regexp"
	"strings"
)

const testVersion = 3

func Abbreviate(l0 string) string {
	l1 := regexp.MustCompile("([\\p{Ll}])([\\p{Lu}])").ReplaceAllString(l0, "$1 $2")

	l2 := regexp.MustCompile("[\\p{P}\\s]").Split(l1, -1)

	l3 := make([]string, 0)

	for i := 0; i < len(l2); i++ {
		runes := []rune(l2[i])
		if len(runes) > 0 {
			l3 = append(l3, string(runes[0]))
		}
	}
	l4 := strings.Join(l3, "")

	l5 := strings.ToUpper(l4)

	return l5
}
