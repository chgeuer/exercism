package react

import (
	"fmt"
)

type updatable interface {
	dependencyUpdated()
	computeValue() int
}

type observable interface {
	subscribe(updatable)
}

type reactor struct {
	inputCells   []*inputCell
	computeCells []updatable
}

type inputCell struct {
	val         int
	subscribers []updatable
}

type computeCell struct {
	val         int
	subscribers []updatable
	callBacks   []func(int)
}

type computeCell1 struct {
	computeCell
	source1          Cell
	computeFunction1 func(int) int
}

type computeCell2 struct {
	computeCell
	source1          Cell
	source2          Cell
	computeFunction2 func(int, int) int
}

// New creates a new Reactor
func New() Reactor {
	return reactor{
		inputCells:   make([]*inputCell, 0),
		computeCells: make([]updatable, 0),
	}
}

func (r reactor) CreateInput(val int) InputCell {
	ic := &inputCell{
		val:         val,
		subscribers: make([]updatable, 0),
	}

	r.inputCells = append(r.inputCells, ic)
	return ic
}

func (r reactor) CreateCompute1(cell Cell, computeFunction func(int) int) ComputeCell {
	cc := &computeCell1{
		computeCell: computeCell{
			subscribers: make([]updatable, 0),
		},
		source1:          cell,
		computeFunction1: computeFunction,
	}
	cell.(observable).subscribe(cc)

	cc.val = cc.computeValue()

	r.computeCells = append(r.computeCells, cc)
	return cc
}

func (r reactor) CreateCompute2(cell1, cell2 Cell, computeFunction func(int, int) int) ComputeCell {
	cc := &computeCell2{
		computeCell: computeCell{
			subscribers: make([]updatable, 0),
		},
		source1:          cell1,
		source2:          cell2,
		computeFunction2: computeFunction,
	}
	cell1.(observable).subscribe(cc)
	cell2.(observable).subscribe(cc)

	cc.val = cc.computeValue()

	r.computeCells = append(r.computeCells, cc)
	return cc
}

func (ic *inputCell) Value() int {
	return ic.val
}

func (ic *inputCell) SetValue(val int) {
	ic.val = val
	for _, cc := range ic.subscribers {
		cc.dependencyUpdated()
	}
}

func (cc *computeCell1) computeValue() int {
	return cc.computeFunction1(cc.source1.Value())
}

func (cc *computeCell2) computeValue() int {
	return cc.computeFunction2(cc.source1.Value(), cc.source2.Value())
}

func (cc *computeCell1) dependencyUpdated() {
	cc.computeCell.update(cc.computeValue())
}

func (cc *computeCell2) dependencyUpdated() {
	cc.computeCell.update(cc.computeValue())
}

func (cc *computeCell) update(newVal int) {
	oldVal := cc.val
	cc.val = newVal
	if oldVal != newVal {
		fmt.Printf("Need to update. Old val %d, new val %d\n", oldVal, newVal)
		for _, c := range cc.callBacks {
			c(newVal)
		}
		for _, c := range cc.subscribers {
			c.dependencyUpdated()
		}
	}
}

func (cc *computeCell) Value() int {
	return cc.val
}

func (cc *computeCell) AddCallback(subscriber func(int)) Canceler {
	cc.callBacks = append(cc.callBacks, subscriber)

	return canceller{
		cc:         cc,
		subscriber: subscriber,
	}
}

type canceller struct {
	cc         *computeCell
	subscriber func(int)
}

func (cnclr canceller) Cancel() {
	toBeRemoved := cnclr.subscriber
	newCallbacks := make([]func(int), 0)
	for _, existingCallback := range cnclr.cc.callBacks {
		if &existingCallback != &toBeRemoved {
			newCallbacks = append(newCallbacks, existingCallback)
		}
	}
	cnclr.cc.callBacks = newCallbacks
}

func (ic *inputCell) subscribe(subscriber updatable) {
	ic.subscribers = append(ic.subscribers, subscriber)
}

func (cc *computeCell1) subscribe(subscriber updatable) {
	cc.subscribers = append(cc.subscribers, subscriber)
}

func (cc *computeCell2) subscribe(subscriber updatable) {
	cc.subscribers = append(cc.subscribers, subscriber)
}
