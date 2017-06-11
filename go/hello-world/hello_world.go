// You can read more about the importance of documentation in Go here:
// https://blog.golang.org/godoc-documenting-go-code
package greeting

// testVersion identifies the version of the test program that you are
// writing your code to. If the test program changes in the future --
// after you have posted this code to the Exercism site -- reviewers
// will see that your code can't necessarily be expected to pass the
// current test suite because it was written to an earlier test version.
//
// This is a convention done for Exercism exercises in the Go language track,
// it is not a requirement of the Go programming language.
//
// This test versioning setup will be common to all the exercises in the
// Go language track. When crafting your own solution file from scratch you
// will be expected to add this constant or the initial test will fail.
// The version number you should use will be found in the constant
// "targetTestVersion" in the test file, see ./hello_test.go for more
// information.
const testVersion = 4

// HelloWorld should have a comment documenting it, beginning with the name of
// the function. It is recommended good practice, but, as we mention above, it's
// not necessary for making your solutions pass.
// https://github.com/golang/go/wiki/CodeReviewComments#comment-sentences
func HelloWorld() string {
	// Write some code here to pass the test suite.

	// When you have a working solution, REMOVE ALL THE STOCK COMMENTS.
	// They're here to help you get started but they only clutter a finished solution.
	// If you leave them in, reviewers will protest!
	return "Hello, World!"
}
