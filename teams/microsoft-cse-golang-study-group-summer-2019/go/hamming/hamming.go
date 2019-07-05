package hamming

import (
	"errors"
)

// Distance computes the Hamming distance between two DNA sequences.
func Distance(a, b string) (int, error) {
	if len(a) != len(b) {
		return -1, errors.New("DNA sequences don't have same size")
	}

	ra := []rune(a)
	rb := []rune(b)
	hammingWeight := 0
	for i := 0; i < len(ra); i++ {
		if ra[i] != rb[i] {
			hammingWeight++
		}
	}

	return hammingWeight, nil
}