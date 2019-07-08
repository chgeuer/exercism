package erratum

import (
	"errors"
	"fmt"
)

// Use the ResourceOpener to handle some string
func Use(resourceOpener ResourceOpener, input string) (e error) {
	var resource Resource
	for {
		resource, e = resourceOpener()
		if e == nil {
			break
		}
		switch e.(type) {
		case TransientError:
			continue
		default:
			return // e is not nill, and not a TransientError
		}
	}
	defer resource.Close()
	defer func() {
		if r := recover(); r != nil {
			switch r.(type) {
			case FrobError:
				fe := FrobError(r.(FrobError))
				resource.Defrob(fe.defrobTag)
				e = fe
			case string:
				e = errors.New(r.(string))
			default:
				e = fmt.Errorf("%+v", r)
			}
			return
		}
	}()

	resource.Frob(input)

	e = nil
	return
}
