package accumulate

const testVersion = 1

func Accumulate(input []string, mapFunction func(string) string) []string {
	result := make([]string, len(input))
	for i := 0; i < len(input); i++ {
		result[i] = mapFunction(input[i])
	}
	return result
}
