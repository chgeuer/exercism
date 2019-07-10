package account

import (
	"sync"
)

type BankAccount struct {
	mutex    *sync.Mutex
	balance  int
	isClosed bool
}

// Open opens a BankAccount
func Open(initialBalance int) *BankAccount {
	if initialBalance < 0 {
		return nil
	}

	return &BankAccount{
		mutex:    &sync.Mutex{},
		balance:  initialBalance,
		isClosed: false,
	}
}

// Close closes a BankAccount
func (a *BankAccount) Close() (int, bool) {
	return a.do(func() (int, bool) {
		a.isClosed = true
		return a.okResponse()
	})
}

// Balance returns the BankAccount balance.
func (a *BankAccount) Balance() (int, bool) {
	return a.do(func() (int, bool) {
		return a.okResponse()
	})
}

// Deposit deposits money on the BankAccount.
func (a *BankAccount) Deposit(amount int) (int, bool) {
	return a.do(func() (int, bool) {
		newAmount := a.balance + amount
		if newAmount < 0 {
			return a.errorResponse()
		}
		a.balance = newAmount
		return a.okResponse()
	})
}

func (a *BankAccount) do(someOperation func() (int, bool)) (int, bool) {
	a.mutex.Lock()
	defer a.mutex.Unlock()

	if a.isClosed {
		return a.errorResponse()
	}

	return someOperation()
}

func (a *BankAccount) okResponse() (int, bool) { return a.balance, true }

func (*BankAccount) errorResponse() (int, bool) { return 0, false }
