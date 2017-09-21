package bob

import (
	"regexp"
	"strings"
)

const testVersion = 3

func Hey(input string) string {
	/*
		"" -> 'Fine. Be that way!'
		question -> 'Sure.'
		yell -> 'Whoa, chill out!
		_ -> 'Whatever.'
	*/
	input = strings.Trim(input, " ")
	if isEmpty(input) {
		return "Fine. Be that way!"
	}
	if isQuestion(input) {
		return "Sure."
	}
	if isShouting(input) {
		return "Whoa, chill out!"
	}
	return "Whatever."
}

func isEmpty(input string) bool {
	return input == ""
}

func isQuestion(input string) bool {
	return strings.HasSuffix(input, "?")
}

func isUppercase(input string) bool {
	return strings.ToUpper(input) == input
}

func isContainsChars(input string) bool {
	r := regexp.MustCompile("^[\\s01234567890,.-]*$")
	return !r.Match([]byte(input))
}

func isShouting(input string) bool {
	return isUppercase(input) && isContainsChars(input)
}
