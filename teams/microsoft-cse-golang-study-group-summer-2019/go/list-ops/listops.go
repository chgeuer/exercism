package listops

// IntList is an integer list.
type IntList []int

type binFunc func(int, int) int

type unaryFunc func(x int) int

type predFunc func(int) bool

// Length returns the length of an IntList (by iterating over the list).
func (inputList IntList) Length() (length int) {
	iterated := false
	for currentIndex := range inputList {
		iterated = true
		length = currentIndex
	}

	if iterated {
		length++
	}
	return
}

// Map maps all elements of the input list.
func (inputList IntList) Map(f unaryFunc) IntList {
	result := make(IntList, inputList.Length())

	for currentIndex, value := range inputList {
		result[currentIndex] = f(value)
	}

	return result
}

// Filter filters all elements of the input list, based on the given predicate.
func (inputList IntList) Filter(p predFunc) IntList {
	result := make([]int, 0, inputList.Length())
	for _, value := range inputList {
		if p(value) {
			result = append(result, value)
		}
	}

	return IntList(result)
}

// Reverse reverses an IntList.
func (inputList IntList) Reverse() IntList {
	len := inputList.Length()
	result := make(IntList, len)

	for i, value := range inputList {
		result[len-i-1] = value
	}

	return result
}

// Append appends elements to a list
func (inputList IntList) Append(additionalList IntList) IntList {
	return inputList.Concat([]IntList{additionalList})
}

// Concat concatenates two lists.
func (inputList IntList) Concat(additionalLists []IntList) IntList {
	len := inputList.Length()
	for _, additionalList := range additionalLists {
		len += additionalList.Length()
	}

	result := make(IntList, len)

	i := 0
	for _, value := range inputList {
		result[i] = value
		i++
	}
	for _, additionalList := range additionalLists {
		for _, value := range additionalList {
			result[i] = value
			i++
		}
	}

	return result
}

// Foldl does a left fold. https://en.wikipedia.org/wiki/Fold_(higher-order_function)
func (inputList IntList) Foldl(f binFunc, initial int) int {
	result := initial
	for _, value := range inputList {
		result = f(result, value)
	}

	return result
}

// Foldr does a right fold. https://en.wikipedia.org/wiki/Fold_(higher-order_function)
func (inputList IntList) Foldr(f binFunc, initial int) int {
	result := initial
	for _, value := range inputList.Reverse() {
		result = f(value, result)
	}

	return result
}
