package raindrops

import (
	"strconv"
	"strings"
)

func Convert(n int) string {
	sli := make([]string, 3)
	if n%3 == 0 {
		sli = append(sli, "Pling")
	}
	if n%5 == 0 {
		sli = append(sli, "Plang")
	}
	if n%7 == 0 {
		sli = append(sli, "Plong")
	}
	plingPlangPlong := strings.Join(sli[:], "")

	if plingPlangPlong != "" {
		return plingPlangPlong
	}
	return strconv.Itoa(n)
}
