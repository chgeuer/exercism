package react

type reactor struct {
}

// New creates a new Reactor
func New() Reactor {
	return reactor{}
}

func (r reactor) CreateInput(int) InputCell {
	return inputCell{}
}

func (r reactor) CreateCompute1(c Cell, compute func(int) int) ComputeCell {
	return computeCell{}
}

func (r reactor) CreateCompute2(cell1, cell2 Cell, compute func(int, int) int) ComputeCell {
	return computeCell{}
}

type inputCell struct {
	val int
}

func (ic inputCell) Value() int {
	return ic.val
}

func (ic inputCell) SetValue(val int) {
	ic.val = val
}

type computeCell struct {
}

func (cc computeCell) Value() int {
	return -1
}

func (cc computeCell) AddCallback(subscriber func(int)) Canceler {
	return canceller{}
}

type canceller struct {
}

func (my canceller) Cancel() {
}
