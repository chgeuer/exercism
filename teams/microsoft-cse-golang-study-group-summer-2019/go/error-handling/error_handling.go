package erratum

import (
	"errors"
	"fmt"
)

// Use the ResourceOpener to handle some string
func Use(o ResourceOpener, input string) (e error) {
	var resourceOpener Resource
	for {
		resourceOpener, e = o()
		if e == nil {
			break
		}
		switch e.(type) {
		case TransientError:
			continue
		default:
			return
		}
	}
	defer resourceOpener.Close()
	defer func() {
		if r := recover(); r != nil {
			switch r.(type) {
			case FrobError:
				fe := FrobError(r.(FrobError))
				resourceOpener.Defrob(fe.defrobTag)
				e = fe
			case string:
				e = errors.New(r.(string))
			default:
				e = fmt.Errorf("%+v", r)
			}
			return
		}
	}()

	resourceOpener.Frob(input)

	e = nil
	return
}
