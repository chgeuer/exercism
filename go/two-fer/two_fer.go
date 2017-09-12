// Package twofer solves a coding exercise.
package twofer

import (
	"fmt"
)

// ShareWith returns a computed string.
func ShareWith(name string) string {
	if len(name) == 0 {
		name = "you"
	}

	return fmt.Sprintf("One for %s, one for me.", name)
}
