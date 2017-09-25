package bob

import (
	"regexp"
	"strings"
)

const testVersion = 3

type RuleAndResponse struct {
	Rule     func(string) bool
	Response string
}

func Hey(input string) string {
	input = strings.Trim(input, " ")

	ruleAndResponses := [...]RuleAndResponse{
		{isEmpty, "Fine. Be that way!"},
		{isShouting, "Whoa, chill out!"},
		{isQuestion, "Sure."},
		{func(_ string) bool { return true }, "Whatever."},
	}

	for _, ruleAndResponse := range ruleAndResponses {
		if ruleAndResponse.Rule(input) {
			return ruleAndResponse.Response
		}
	}

	return ""
}

func isEmpty(input string) bool {
	return input == ""
}

func isQuestion(input string) bool {
	return strings.HasSuffix(input, "?")
}

func isShouting(input string) bool {
	isUppercase := strings.ToUpper(input) == input

	r := regexp.MustCompile("^[\\s01234567890,.-]*$")
	isContainsChars := !r.Match([]byte(input))

	return isUppercase && isContainsChars
}
