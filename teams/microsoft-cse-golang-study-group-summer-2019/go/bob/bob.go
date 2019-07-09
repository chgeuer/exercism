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
		{isYellingQuestion, "Calm down, I know what I'm doing!"},
		{isYelling, "Whoa, chill out!"},
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
	return regexp.MustCompile("^\\s*$").Match([]byte(input))
}

func isYellingQuestion(input string) bool {
	return isYelling(input) && isQuestion(input)
}

func isYelling(input string) bool {
	return isUppercase(input) && containsCharacters(input)
}

func isQuestion(input string) bool {
	return strings.HasSuffix(input, "?")
}

func isUppercase(input string) bool {
	return strings.ToUpper(input) == input
}

func containsCharacters(input string) bool {
	return regexp.MustCompile("\\pL").Match([]byte(input))
}
