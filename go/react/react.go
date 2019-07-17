package react

import (
	"fmt"
	"sync"
)

type computable interface {
	update()
	commit(commitID int)
}

type observable interface {
	subscribe(computable)
}

var (
	commitCount = 0
	mutex       = &sync.Mutex{}
)

func nextCommitID() int {
	mutex.Lock()
	defer mutex.Unlock()
	commitCount++
	return commitCount
}

type reactor struct{}

// New creates a new Reactor
func New() Reactor {
	return reactor{}
}

func (r reactor) CreateInput(val int) InputCell {
	ic := &inputCell{
		val:         val,
		subscribers: make([]computable, 0),
	}

	// fmt.Printf("input %d\n", ic.val)

	return ic
}

func (r reactor) CreateCompute1(cell Cell, computeFunction func(int) int) ComputeCell {
	cc := &computeCell1{
		computeCell: computeCell{
			subscribers: make([]computable, 0),
			val:         computeFunction(cell.Value()),
		},
		source1:          cell,
		computeFunction1: computeFunction,
	}
	cell.(observable).subscribe(cc)

	return cc
}

func (r reactor) CreateCompute2(cell1, cell2 Cell, computeFunction func(int, int) int) ComputeCell {
	cc := &computeCell2{
		computeCell: computeCell{
			subscribers: make([]computable, 0),
			val:         computeFunction(cell1.Value(), cell2.Value()),
		},
		source1:          cell1,
		source2:          cell2,
		computeFunction2: computeFunction,
	}
	cell1.(observable).subscribe(cc)
	cell2.(observable).subscribe(cc)

	return cc
}

type inputCell struct {
	subscribers []computable
	val         int
}

func (ic *inputCell) Value() int {
	return ic.val
}

func (ic *inputCell) SetValue(newVal int) {
	if ic.val != newVal {
		// fmt.Printf("inputCell SetValue Old val %d, new val %d\n", ic.val, newVal)
		ic.val = newVal
		commitID := nextCommitID()
		for _, c := range ic.subscribers {
			c.update()
		}
		for _, c := range ic.subscribers {
			c.commit(commitID)
		}
	}
}

func (ic *inputCell) subscribe(subscriber computable) {
	ic.subscribers = append(ic.subscribers, subscriber)
}

type computeCell struct {
	val             int
	valChanged      bool
	subscribers     []computable
	currentCommitID int
	callBacks       []*func(int)
}

func (cc *computeCell) AddCallback(subscriber func(int)) Canceler {
	cc.callBacks = append(cc.callBacks, &subscriber)
	return canceller{
		cc:         cc,
		subscriber: &subscriber,
	}
}

type computeCell1 struct {
	computeCell
	source1          Cell
	computeFunction1 func(int) int
}

func (cc *computeCell1) update() {
	newVal := cc.computeFunction1(cc.source1.Value())
	cc.valChanged = cc.val != newVal
	cc.val = newVal
	if cc.valChanged {
		for _, c := range cc.subscribers {
			c.update()
		}
	}
}

func (cc *computeCell2) update() {
	newVal := cc.computeFunction2(cc.source1.Value(), cc.source2.Value())

	fmt.Printf("computeCell2.update() val=%d newVal=%d cell1=%d cell2=%d\n", cc.val, newVal, cc.source1.Value(), cc.source2.Value())

	cc.valChanged = cc.val != newVal
	cc.val = newVal
	if cc.valChanged {
		for _, c := range cc.subscribers {
			c.update()
		}
	}
}

func (cc *computeCell1) commit(commitID int) {
	if cc.valChanged {
		cc.valChanged = false
		for _, c := range cc.subscribers {
			c.commit(commitID)
		}
		if cc.currentCommitID != commitID {
			cc.currentCommitID = commitID
			for _, cb := range cc.callBacks {
				(*cb)(cc.val)
			}
		}
	}
}

func (cc *computeCell2) commit(commitID int) {
	fmt.Printf("computeCell2.commit(%d) valChanged %v\n", commitID, cc.valChanged)
	if cc.valChanged {
		cc.valChanged = false
		for _, c := range cc.subscribers {
			c.commit(commitID)
		}
		if cc.currentCommitID != commitID {
			cc.currentCommitID = commitID
			for _, cb := range cc.callBacks {
				(*cb)(cc.val)
			}
		}
	}
}

func (cc *computeCell1) Value() int {
	return cc.val
}

func (cc *computeCell1) subscribe(subscriber computable) {
	cc.subscribers = append(cc.subscribers, subscriber)
}

type computeCell2 struct {
	computeCell
	source1          Cell
	source2          Cell
	computeFunction2 func(int, int) int
}

func (cc *computeCell2) Value() int {
	return cc.val
}

func (cc *computeCell2) subscribe(subscriber computable) {
	cc.subscribers = append(cc.subscribers, subscriber)
}

type canceller struct {
	cc         *computeCell
	subscriber *func(int)
}

func (cnclr canceller) Cancel() {
	toBeRemoved := cnclr.subscriber
	newCallbacks := make([]*func(int), 0)
	for _, existingCallback := range cnclr.cc.callBacks {
		if existingCallback != toBeRemoved {
			newCallbacks = append(newCallbacks, existingCallback)
		}
	}
	cnclr.cc.callBacks = newCallbacks
}
