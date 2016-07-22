
λAMBDA
    a terse, minimal, functional programming language


x := a 
 - assignment

\n:\y:n*y
 - lambdas

(2 * 3 > 5) ->
	"math is alright"
	"something is wrong"
 - control flow

[1 2 3]/-1
 - lists


fibn := \n:
	(n > 2) ->
		(n < 25) -> {
			(fibn:(n - 1)) + (fibn:(n - 2))
		}
			"too big"
		1

fastfibn := \n:{
	fibs := [1 1]
	addfib := \_: {
		fibs := fibs + [fibs/-1 + fibs/-2]
		((fibs/n) = nil) ->
			addfib:_
			fibs/n
	}
	addfib:_
}		

print:fibn:10
print:fastfibn:40
