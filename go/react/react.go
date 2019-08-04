package react

import (
	"sync"
)

type computable interface {
	preUpdate(source Cell, changeID int)
	commitUpdate(source Cell, changeID int)
}

type observable interface {
	subscribe(computable)
}

type reactor struct{}

type inputCell struct {
	cellID      int
	subscribers []computable
	val         int
}

type computeCell struct {
	cellID          int
	val             int
	updateInFlight  bool
	valChanged      bool
	subscribers     []computable
	currentchangeID int
	callBacks       []*func(int)
}

type computeCell1 struct {
	computeCell
	candidate1       int
	source1          Cell
	computeFunction1 func(int) int
}

type computeCell2 struct {
	computeCell
	candidate1       int
	candidate2       int
	source1          Cell
	source2          Cell
	computeFunction2 func(int, int) int
}

var (
	changeID = 0
	cellID   = 0
	mutex    = &sync.Mutex{}
)

func nextChangeID() int {
	mutex.Lock()
	defer mutex.Unlock()
	changeID++
	return changeID
}

func nextCellID() int {
	mutex.Lock()
	defer mutex.Unlock()
	cellID++
	return cellID
}

// New creates a new Reactor
func New() Reactor {
	return reactor{}
}

func (r reactor) CreateInput(val int) InputCell {
	ic := &inputCell{
		cellID:      nextCellID(),
		val:         val,
		subscribers: make([]computable, 0),
	}

	return ic
}

func (r reactor) CreateCompute1(cell1 Cell, computeFunction func(int) int) ComputeCell {
	v1 := cell1.Value()
	cc := &computeCell1{
		computeCell: computeCell{
			cellID:      nextCellID(),
			subscribers: make([]computable, 0),
			val:         computeFunction(v1),
		},
		candidate1:       v1,
		source1:          cell1,
		computeFunction1: computeFunction,
	}
	cell1.(observable).subscribe(cc)

	return cc
}

func (r reactor) CreateCompute2(cell1, cell2 Cell, computeFunction func(int, int) int) ComputeCell {
	v1 := cell1.Value()
	v2 := cell2.Value()
	v := computeFunction(v1, v2)
	cc := &computeCell2{
		computeCell: computeCell{
			cellID:         nextCellID(),
			subscribers:    make([]computable, 0),
			val:            v,
			valChanged:     false,
			updateInFlight: false,
		},
		candidate1:       v1,
		candidate2:       v2,
		source1:          cell1,
		source2:          cell2,
		computeFunction2: computeFunction,
	}

	cell1.(observable).subscribe(cc)
	cell2.(observable).subscribe(cc)

	return cc
}

func (ic *inputCell) Value() int {
	return ic.val
}

func (ic *inputCell) SetValue(newVal int) {
	if ic.val != newVal {
		ic.val = newVal
		changeID := nextChangeID()
		for _, c := range ic.subscribers {
			c.preUpdate(ic, changeID)
		}
		for _, c := range ic.subscribers {
			c.commitUpdate(ic, changeID)
		}
	}
}

func (ic *inputCell) subscribe(subscriber computable) {
	ic.subscribers = append(ic.subscribers, subscriber)
}

func (cc *computeCell) AddCallback(subscriber func(int)) Canceler {
	cc.callBacks = append(cc.callBacks, &subscriber)
	return canceller{
		cc:         cc,
		subscriber: &subscriber,
	}
}

func ID(cell Cell) int {
	switch cell.(type) {
	case *inputCell:
		return cell.(*inputCell).cellID
	case *computeCell1:
		return cell.(*computeCell1).cellID
	case *computeCell2:
		return cell.(*computeCell2).cellID
	default:
		return -1
	}
}

func (cc *computeCell1) Value() int {
	if cc.updateInFlight {
		return cc.computeFunction1(cc.candidate1)
	}
	return cc.val
}

func (cc *computeCell2) Value() int {
	if cc.updateInFlight {
		return cc.computeFunction2(cc.candidate1, cc.candidate2)
	}
	return cc.val
}

func (cc *computeCell1) preUpdate(source Cell, changeID int) {
	cc.candidate1 = cc.source1.Value()
	cc.valChanged = cc.val != cc.computeFunction1(cc.candidate1)
	cc.updateInFlight = true

	if cc.valChanged {
		for _, c := range cc.subscribers {
			c.preUpdate(cc, changeID)
		}
	}
}

func (cc *computeCell1) commitUpdate(source Cell, changeID int) {
	if cc.valChanged {
		cc.val = cc.computeFunction1(cc.candidate1)
		cc.updateInFlight = false
		cc.valChanged = false

		for _, c := range cc.subscribers {
			c.commitUpdate(cc, changeID)
		}
		if cc.currentchangeID != changeID {
			cc.currentchangeID = changeID
			for _, cb := range cc.callBacks {
				(*cb)(cc.val)
			}
		}
	}
}

func (cc *computeCell2) preUpdate(source Cell, changeID int) {
	if source == cc.source1 {
		cc.candidate1 = cc.source1.Value()
	}
	if source == cc.source2 {
		cc.candidate2 = cc.source2.Value()
	}
	cc.valChanged = cc.val != cc.computeFunction2(cc.candidate1, cc.candidate2)
	cc.updateInFlight = true

	if cc.valChanged {
		for _, c := range cc.subscribers {
			c.preUpdate(cc, changeID)
		}
	}
}

func (cc *computeCell2) commitUpdate(source Cell, changeID int) {
	if cc.valChanged {
		cc.val = cc.computeFunction2(cc.candidate1, cc.candidate2)
		cc.updateInFlight = false
		cc.valChanged = false
		for _, c := range cc.subscribers {
			c.commitUpdate(cc, changeID)
		}
		if cc.currentchangeID != changeID {
			cc.currentchangeID = changeID
			for _, cb := range cc.callBacks {
				(*cb)(cc.val)
			}
		}
	}
}

func (cc *computeCell1) subscribe(subscriber computable) {
	cc.subscribers = append(cc.subscribers, subscriber)
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
